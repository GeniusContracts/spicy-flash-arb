def swap_quipu_to_spicy(pool_quipu, spicy_address, pool,spicy, pool_address, token_contract, token_address, token, wtz, amount):

    dead = calendar.timegm(time.gmtime())+1800
    pytezos.bulk(
        pool_quipu.tezToTokenPayment(min_out=token, receiver=address).with_amount(Decimal(amount)),

        token_contract.transfer([{"txs":[{"to_": spicy_address, "amount": token, "token_id": 0}], "from_": address}]),
        contract_pool.start_swap(_to=address, flash=None, amount0Out=0, amount1Out=round(token*0.997))
    ).send()

def swap_spicy_to_quipu(pool_quipu, spicy_address, pool_spicy, pool_address, token_contract, token_address, token, xtz, amount):

    dead = calendar.timegm(time.gmtime())+1800
    pytezos.bulk(
        wtz_contract.transfer([{"txs":[{"to_": spicy_address, "amount": amount, "token_id": 0}], "from_": address}]),
        pool_spicy.start_swap(_to=address, flash=None, amount0Out=0, amount1Out=round(token*0.997)),

        token_contract.update_operators([{"add_operator": {"owner": address, "operator": pool_address, "token_id": 0}}]),
        pool_quipu.tokenToTezPayment(amount = token, min_out = round(xtz*0.997), receiver = address)
    ).send()
