# spicy-flash-arb
LIGO Contracts for flash arbitrage of SpicySwap pools

Dirty contract, untested. But this stuff is 0-collateral so its less of a big deal.

```
ligo compile-contract flasher_s2q.mligo flash_main > ./flasher_s2q.tz

tezos-client originate contract flasher_s2q transferring 0 from $ADDRESS running ./flasher_s2q.tz --init 'None' --burn-cap 3
```
