(* #include "../../fa2/fa2_interface.mligo"
#include "../../fa2_modules/admin/non_pausable_simple_admin.mligo" *)
#include "./fa2_token_manager.mligo"
#include "fa_currency_lib.mligo"

(* ----------- TYPES -------------- *)

type rewards_recieved = [@layout:comb] {
  amount : nat;
  pool : address;
  timestamp : timestamp;
}

type fa2_token =
  [@layout:comb]
  {
    token_id : token_id;
    amount : nat;
  }

type tokens =
  [@layout:comb]
  {
    fa2_address : address;
    fa2_batch : (fa2_token list);
  }

type farm_params = [@layout:comb] {
  reward_per_sec: nat;
}

type fa2_currency = [@layout:comb] {
  fa2_address : address;
  token_id : token_id;
}

type wrap_params = (address)

type unwrap_params = (nat * address)

type qiupu_token_to_tez = [@layout:comb] {
  amount: nat;
  min_out: nat;
  receiver: address
}

type qiupu_tez_to_token = [@layout:comb] {
  min_out: nat;
  receiver: address
}


type flash_params = [@layout:comb] {
  amount0Out : nat;
  amount1Out : nat;
}


type swap_request = {
  amount0Out : nat;
  amount1Out : nat;
  _to : address;
  flash : flash_params contract option;
}

type flash_execution = [@layout:comb] {
  
  _to: address; (* usually caller *)

  flash_from: address; (* usually biggest TVL pool, deepest WTZ reserves *)
  flash_token_0: bool; (* can't get this info on chain so tell me which one is WTZ *)
  flash_amount: nat; (* calc *)

  q_pool: address; (* for calling tezToToken *)
  q_min_out: nat; (* calc, should be 0 slippage *)
  q_amount: nat; (* calc *)

  (* this can handle FA1.2 or FA2 *)
  arb_token: fa_currency; (* should be the token that sits in between the arb, of the form [WTZ -> *token* -> WTZ] *)

  s_pool: address; (* for calling start_swap *)
  s_amount_out_0: nat; (* remember, contract deosn't know which token is 0 or 1 *)
  s_amount_out_1: nat; (* remember, contract deosn't know which token is 0 or 1 *)

  to_xtz: bool; (*this call will make WTZ by default, adding to_xtz converts back to XTZ at 0.1% fee  *)
}

type storage = [@layout:comb] {
  state: flash_execution option;
}

type return = operation list * storage


type flash_main =
  | Flash_Q2S of flash_execution
  | Flash_Q2S_Reciever of flash_params
  | Flash_Q2S_Finish of unit
  | Flash_S2Q of flash_execution
  | Flash_S2Q_Reciever of flash_params
  | Flash_S2Q_Reciever_Balance of balance_of_response list


let fail_if_state (s : storage) : unit =
  match (s.state) with
      None -> unit
    | Some _ -> (failwith "FLASH_IN_PROGESS" : unit)

let fail_if_not_state (s : storage) : unit =
  match (s.state) with
      None -> (failwith "NO_FLASH" : unit)
    | Some _ -> unit

let get_self_bal_recieve (add : address) : ((balance_of_response list) contract) = 
  let c : (balance_of_response list) contract option = Tezos.get_entrypoint_opt "%flash_s2q_recieve_balance" add in
  match c with
    None -> (failwith "FATAL_WTZ_BALANCE_RECIEVE" : (balance_of_response list) contract)
  | Some c ->  c

let assert_msg (condition, msg : bool * string ) : unit =
  if (not condition) then failwith(msg) else unit


let drain_or_unwrap_remaining_wtz (to_xtz, amt, reciever, self:  bool * nat * address * address) : operation =
  
  if(to_xtz = false) then (* drain *)

    let wtz_token : fa_currency = {
      fa2_address = ("KT1PnUZCp3u2KzWr93pn4DD7HAJnm3rWVrgn" : address);
      token_id = (Some 0n);
    } in

    fa_transfer(self, reciever, amt, wtz_token)

  else (* unwrap *)
    let unwrap : unwrap_params contract option = Tezos.get_entrypoint_opt "%unwrap" ("KT1Pyd1r9F4nMaHy8pPZxPSq6VCn9hVbVrf4" : address) in

    match unwrap with
      None -> (failwith "NOTHING_TO_UNWRAP" : operation)
    | Some wrap_ctr -> Tezos.transaction (amt, reciever) 0mutez wrap_ctr


(*  *)
(* 

QUIPI TO SPICY

Flash WTZ -> XTZ -> token -> WTZ [ -> XTZ (Optional) ]

*)
(*  *)


let flash_q2s (f, s, self : flash_execution * storage * address) : (operation list) * storage = begin

(*   
  EXECUTES:
  -Flash WTZ
  -Unwrap to XTZ
  -tezToToken
  -tx token to spicy
  -start_swap, token to WTZ
  -Pay back Flash
  -Leftover WTZ back to caller, or unwrapped to XTZ and to caller
 *)

  let () : unit = fail_if_state(s) in

  (* will execute flash_q2s_recieve *)
  let spicy_pool : swap_request contract = 
    match (Tezos.get_entrypoint_opt "%start_swap" f.flash_from : swap_request contract option) with
      None -> (failwith "FATAL_PAIR_NOT_FOUND" : swap_request contract)
    | Some c ->  c in

  let (amount0Out, amount1Out) : (nat * nat) = 
    if f.flash_token_0 
      then (f.flash_amount, 0n)
    else   (0n, f.flash_amount)
  in

  (* spicy will call back to self *)
  let recieve_flash : flash_params contract option = (Tezos.get_entrypoint_opt "%flash_Q2S_Reciever" self : flash_params contract option) in

  let req : swap_request = {
    amount0Out = amount0Out;
    amount1Out = amount1Out;
    _to = self;
    flash = recieve_flash;
  } in

  let flash_request_to_spicy : operation = Tezos.transaction req 0mutez spicy_pool in

  ([flash_request_to_spicy], { s with state = (Some f); })
end



let flash_q2s_recieve (fp, s, self, self_callback : flash_params * storage * address * (flash_main contract) ) : (operation list) * storage = begin

  let () : unit = fail_if_not_state(s) in

  let wtz_amt_flashed = 
    if(fp.amount0Out > 0n) 
      then fp.amount0Out
    else
      fp.amount1Out
  in

  (* unwrap XTZ to this contract *)
  let unwrap : unwrap_params contract option = Tezos.get_entrypoint_opt "%unwrap" ("KT1Pyd1r9F4nMaHy8pPZxPSq6VCn9hVbVrf4" : address) in
  
  let unwrap_operation : operation = 
  match unwrap with
    None -> (failwith "NOTHING_TO_UNWRAP" : operation)
  | Some c -> Tezos.transaction (wtz_amt_flashed, self) 0mutez c
  in

  let param : flash_main = Flash_Q2S_Finish(unit) in

  (* self-call, that assumes XTZ in contract. send params *)
  let finish_flash : operation = Tezos.transaction param 0mutez self_callback in

  (unwrap_operation::[finish_flash], s)
end


let flash_q2s_finish (s, self : storage * address ) : (operation list) * storage = begin

  let () : unit = fail_if_not_state(s) in

  let f : flash_execution = match s.state with
      None -> (failwith "FATAL" : flash_execution)
    | Some flash_exec_params -> flash_exec_params 
  in

  let wtz_token : fa_currency = {
    fa2_address = ("KT1PnUZCp3u2KzWr93pn4DD7HAJnm3rWVrgn" : address);
      token_id = (Some 0n);
  }
  in

  (* at this point its assumbed that the contract is holding some XTZ from the unwrap action *)

  (* swap tez for tokens from quip *)
  let q_pool : qiupu_tez_to_token contract = 
    match (Tezos.get_entrypoint_opt "%tezToTokenPayment" f.q_pool : qiupu_tez_to_token contract option) with
      None -> (failwith "FATAL_PAIR_NOT_FOUND" : qiupu_tez_to_token contract)
    | Some c ->  c in

  (* amounts calculated off-chain *)
  let q_req : qiupu_tez_to_token = {
    min_out = f.q_min_out;
    receiver = self;
  } in

  let q_amt : tez = (f.q_amount * 1mutez) in

  (* exec *)
  let q_tez_to_token : operation = Tezos.transaction q_req q_amt q_pool in

  (* swap tokens for wtz on spicy *)
  let tx_token_to_spicy : operation = fa_transfer(self, f.s_pool, f.q_min_out, f.arb_token) in

  (* get pool *)
  let spicy_pool : swap_request contract = 
    match (Tezos.get_entrypoint_opt "%start_swap" f.s_pool : swap_request contract option) with
      None -> (failwith "FATAL_PAIR_NOT_FOUND" : swap_request contract)
    | Some c ->  c in

  (* amounts calculated off-chain *)
  let s_req : swap_request = {
    amount0Out = f.s_amount_out_0;
    amount1Out = f.s_amount_out_1;
    _to = self;
    flash = (None : flash_params contract option);
  } in

  (* exec *)
  let s_token_to_wtz : operation = Tezos.transaction s_req 0mutez spicy_pool in

  (* should always be WTZ out *)
  let final_wtz_balance : nat = 
    if(f.s_amount_out_0 > 0n) 
      then f.s_amount_out_0
    else
      f.s_amount_out_1
  in

  (* at this point its assumed that the contract is holding some WTZ from the arb *)
  
  (* fee from original flash swap *)
  let fee : nat = ((f.flash_amount * 3n) / 997n) + 1n in
  let wtz_payback_amount : nat = f.flash_amount + fee in

  (* exec *)
  let tx_wtz_to_spicy : operation = fa_transfer(self, f.s_pool, wtz_payback_amount, wtz_token) in

  (* calc amount left over *)
  let wtz_gains : nat = match is_nat(final_wtz_balance - wtz_payback_amount) with
    None -> (failwith "FATAL_GAINS_SUB_OVERFLOW" : nat) (* it will usually fail here if your trade didn't generate enough WTZ to pay back *)
  | Some n -> n in

  (* always drain the WTZ to 0 *)
  let extract_gains : operation = drain_or_unwrap_remaining_wtz(f.to_xtz, wtz_gains, f._to, self) in
  
  (* Fin *)
  (q_tez_to_token::tx_token_to_spicy::s_token_to_wtz::tx_wtz_to_spicy::[extract_gains], { s with state = (None : flash_execution option); })
end




(*  *)
(* 

SPICY TO QUIPI 

Flash WTZ -> token -> XTZ -> WTZ -> (Optional) XTZ  

*)
(*  *)


(* Step 1 *)
let flash_s2q (f, s, self : flash_execution * storage * address ) : (operation list) * storage = begin

  (*   
    EXECUTES:
    -Flash WTZ

    -tx WTZ to spicy
    -start_swap, WTZ to some Token
    
    -updates operators (FA1.2 or FA2)
    -tokenToTez
    -removes operators (FA1.2 or FA2)

    -Pay back Flash, by wrapping XTZ to do so
    -Leftover XTZ back to caller, or wrapped to WTZ and to caller
   *)

  let () : unit = fail_if_state(s) in

  (* will execute flash_s2q_recieve *)
  let spicy_pool : swap_request contract = 
    match (Tezos.get_entrypoint_opt "%start_swap" f.flash_from : swap_request contract option) with
      None -> (failwith "FATAL_PAIR_NOT_FOUND" : swap_request contract)
    | Some c ->  c in

  let (amount0Out, amount1Out) : (nat * nat) = 
    if f.flash_token_0 
      then (f.flash_amount, 0n)
    else   (0n, f.flash_amount)
  in

  (* spicy will call back to self *)
  let recieve_flash : flash_params contract option = (Tezos.get_entrypoint_opt "%flash_S2Q_Reciever" self : flash_params contract option) in

  let req : swap_request = {
    amount0Out = amount0Out;
    amount1Out = amount1Out;
    _to = self;
    flash = recieve_flash;
  } in

  let flash_request_to_spicy : operation = Tezos.transaction req 0mutez spicy_pool in

  ([flash_request_to_spicy], { s with state = (Some f); })
end

(* Step 2 *)
let flash_s2q_recieve (fp, s, self : flash_params * storage * address ) : (operation list) * storage = begin

  let () : unit = fail_if_not_state(s) in

  let f : flash_execution = match s.state with
      None -> (failwith "FATAL" : flash_execution)
    | Some flash_exec_params -> flash_exec_params 
  in

  let wtz_token : fa_currency = {
    fa2_address = ("KT1PnUZCp3u2KzWr93pn4DD7HAJnm3rWVrgn" : address);
      token_id = (Some 0n);
  } in 

  let wtz_amt_flashed = 
    if(fp.amount0Out > 0n) 
      then fp.amount0Out
    else 
      fp.amount1Out
  in

  (* at this point its assumed that the contract is holding some WTZ from the flash action *)

  (* swap wtz for tokens on spicy *)
  let tx_wtz_to_spicy : operation = fa_transfer(self, f.s_pool, wtz_amt_flashed, wtz_token) in

  (* get pool *)
  let spicy_pool : swap_request contract = 
    match (Tezos.get_entrypoint_opt "%start_swap" f.s_pool : swap_request contract option) with
      None -> (failwith "FATAL_PAIR_NOT_FOUND" : swap_request contract)
    | Some c -> c in

  (* amounts calculated off-chain *)
  let s_req : swap_request = {
    amount0Out = f.s_amount_out_0;
    amount1Out = f.s_amount_out_1;
    _to = self;
    flash = (None : flash_params contract option);
  } in

  (* exec *)
  let s_wtz_to_token : operation = Tezos.transaction s_req 0mutez spicy_pool in

  (* swap tokens for tez from quip *)
  let upd_op : operation = fa_approve(self, f.q_pool, f.q_amount, f.arb_token) in
  let rem_op : operation = fa_approve(self, f.q_pool, 0n, f.arb_token) in 

  (* swap tokens for tez from quip *)
  let q_pool : qiupu_token_to_tez contract = 
    match (Tezos.get_entrypoint_opt "%tokenToTezPayment" f.q_pool : qiupu_token_to_tez contract option) with
      None -> (failwith "FATAL_PAIR_NOT_FOUND" : qiupu_token_to_tez contract)
    | Some c ->  c in

  (* amounts calculated off-chain *)
  let q_req : qiupu_token_to_tez = {
    amount = f.q_amount;
    min_out = f.q_min_out;
    receiver = self;
  } in

  let q_amt : tez = (f.q_amount * 1mutez) in

  (* exec *)
  let q_token_to_tez : operation = Tezos.transaction q_req 0mutez q_pool in

  (* at this point its assumed that the contract is holding some XTZ from the arbitrage action *)

  let final_xtz_balance : tez = f.q_min_out * 1mutez in

  (* but wtf. i cant tell how much this XTZ is worth in WTZ *)
  (* So we wrap all and see how much WTZ was gained. Unfortunate but whatever. *)

  let wrap : wrap_params contract option = Tezos.get_entrypoint_opt "%wrap" ("KT1Pyd1r9F4nMaHy8pPZxPSq6VCn9hVbVrf4" : address) in

  (* wrapping is free, so we send directly *)
  let wrap_xtz_from_quipi : operation = match wrap with
    None -> (failwith "NOTHING_TO_WRAP" : operation)
  | Some wrap_ctr -> Tezos.transaction self final_xtz_balance wrap_ctr
  in

  let fee : nat = ((wtz_amt_flashed * 3n) / 997n) + 1n in
  let wtz_payback_amount : nat = wtz_amt_flashed + fee in

  let tx_wtz_to_spicy : operation = fa_transfer(self, f.s_pool, wtz_payback_amount, wtz_token) in

  (* query balance of WTZ *)
  let self_bal_contract : (balance_of_response list) contract option = Tezos.get_entrypoint_opt "%flash_S2Q_Reciever_Balance" self in
  
  let self_bal : (balance_of_response list) contract = match self_bal_contract with
    None -> (failwith "NO_BAL_RECIEVER" : (balance_of_response list) contract)
  | Some c ->  c
  in

  let c = address_to_contract_balance_entrypoint(wtz_token.fa2_address) in

  let get_balance_and_reenter : operation = Tezos.transaction {
    requests = [{
      owner = self;
      token_id = 0n
    }];
    callback = self_bal
  } 0mutez c
  in

  (tx_wtz_to_spicy::s_wtz_to_token::upd_op::q_token_to_tez::rem_op::wrap_xtz_from_quipi::tx_wtz_to_spicy::[get_balance_and_reenter], s)
end

(* Step 3 *)
let flash_s2q_recieve_balance (b, s, self : balance_of_response list * storage * address) : (operation list) * storage = begin

  let () : unit = fail_if_not_state(s) in

  let f : flash_execution = match s.state with
      None -> (failwith "FATAL" : flash_execution)
    | Some flash_exec_params -> flash_exec_params 
  in

  let wtz_token : fa_currency = {
    fa2_address = ("KT1PnUZCp3u2KzWr93pn4DD7HAJnm3rWVrgn" : address);
    token_id = (Some 0n);
  } in 

  (* almost unnecessary with 0 collateral at stake *)
  let () = assert_msg (Tezos.sender = wtz_token.fa2_address, "NOT_WTZ") in

  let final_wtz_balance = match b with
    | [] -> (failwith "NO_BALANCE" : balance_of_response)
    | bal :: _ -> bal
  in

  let () = assert_msg (self = final_wtz_balance.request.owner, "NOT_SELF") in

  (* at this point its assumed that the contract is holding some WTZ from the wrap *)

  (* flash swapped pool is already paid back. *)

  (* always drain the WTZ to 0 *)
  let extract_gains : operation = drain_or_unwrap_remaining_wtz(f.to_xtz, final_wtz_balance.balance, f._to, self) in
  
  (* Fin *)
  ([extract_gains], { s with state = (None : flash_execution option); })
end

let flash_main (param, storage : flash_main * storage) : return = begin    
  let self : address = Tezos.self_address in
  let this_ctr : (flash_main contract) = (Tezos.self("%default") : flash_main contract) in

  match param with
  | Flash_Q2S f -> flash_q2s(f, storage, self)
  | Flash_Q2S_Reciever fp -> flash_q2s_recieve(fp, storage, self, this_ctr)
  | Flash_Q2S_Finish _ -> flash_q2s_finish(storage, self)
  | Flash_S2Q f -> flash_s2q(f, storage, self)
  | Flash_S2Q_Reciever fp -> flash_s2q_recieve(fp, storage, self)
  | Flash_S2Q_Reciever_Balance bal -> flash_s2q_recieve_balance(bal, storage, self)
end


