from datetime import datetime
from pytezos import pytezos
from sympy import *
import requests
import time

pytezos = pytezos.using(key = 'PRIVATE_KEY', shell='https://rpc.tzbeta.net')

quipu_entrypoints = ['tezToTokenPayment','tokenToTezPayment']
spicy_entrypoints = ['swap_exact_for_tokens']

contracts = []

pairs  = {
        0: {
            'name': 'USDtz',
            'quipu_address': 'KT1WxgZ1ZSfMgmsSDDcUn8Xn577HwnQ7e1Lb',
            'spicy_address': 'KT1KgNVokovu4dSBFZXmFXgUni5TypwMBbRS',
            'quipu_pool': pytezos.contract('KT1WxgZ1ZSfMgmsSDDcUn8Xn577HwnQ7e1Lb'),
            'spicy_pool': pytezos.contract('KT1KgNVokovu4dSBFZXmFXgUni5TypwMBbRS'),
            'decimal': 6
            },
        1: {
            'name': 'uUSD',
            'quipu_address': 'KT1EtjRRCBC2exyCRXz8UfV7jz7svnkqi7di',
            'spicy_address': 'KT1UefQz7nP8BuuDk5Dd5LWo22N1ZPB7JB5o',
            'quipu_pool': pytezos.contract('KT1EtjRRCBC2exyCRXz8UfV7jz7svnkqi7di'),
            'spicy_pool': pytezos.contract('KT1UefQz7nP8BuuDk5Dd5LWo22N1ZPB7JB5o'),
            'decimal': 12
            },
        2: {
            'name': 'SPI',
            'quipu_address': 'KT1Eg2QesN1tCzScTrvoeKm5W67GgjV32McR',
            'spicy_address': 'KT1UUjqN2tVHb2xEXS6XHs1GbuV1F6cDTAiT',
            'quipu_pool': pytezos.contract('KT1Eg2QesN1tCzScTrvoeKm5W67GgjV32McR'),
            'spicy_pool': pytezos.contract('KT1UUjqN2tVHb2xEXS6XHs1GbuV1F6cDTAiT'),
            'decimal': 6
            },
        3: {
            'name': 'kUSD',
            'quipu_address': 'KT1K4EwTpbvYN9agJdjpyJm4ZZdhpUNKB3F6',
            'spicy_address': 'KT1NN1NgmKFTW5FUWiyxVjUt3kH9bCiqgxLW',
            'quipu_pool': pytezos.contract('KT1K4EwTpbvYN9agJdjpyJm4ZZdhpUNKB3F6'),
            'spicy_pool': pytezos.contract('KT1NN1NgmKFTW5FUWiyxVjUt3kH9bCiqgxLW'),
            'decimal': 18
            },
        4: {
            'name': 'MTRIA',
            'quipu_address': 'KT1FptuULGK69mZRsBz62CSFdRs52etEb6Ah',
            'spicy_address': 'KT1RBLbqbdej7xy1bbqb4pG7YQJxFxQhc42Z',
            'quipu_pool': pytezos.contract('KT1FptuULGK69mZRsBz62CSFdRs52etEb6Ah'),
            'spicy_pool': pytezos.contract('KT1RBLbqbdej7xy1bbqb4pG7YQJxFxQhc42Z'),
            'decimal': 6
            }
        }

def return_on_swap(target, source, amount):
    #return on swap

    return (target*amount/(source+amount))*0.997

def to_decimal(n, decimal):
    #convert int to decimal for calculation

    n = list(str(n))
    n.insert(decimal*-1, '.')
    return float(''.join(n))

def formate_decimal(n, decimal):
    #Limite le nombre de decimal

    n = str(n)
    return float(n[:n.find('.')+decimal+1])

def local_maxima(sr0, sr1, tr1, tr0):
    #calcul best profit on arb
    #sr: source reserve 
    #tr: target reserve

    x = Symbol('x')
    f = (tr0*((sr1*x/(sr0+x))*0.997)/(tr1+((sr1*x/(sr0+x))*0.997)))*0.997-x
    fprime = diff(f, x)
    return [[xx, f.subs(x, xx)] for xx in solve(fprime, x) if(xx>0 and f.subs(x, xx)> 0.0001)]

def parse_spicy(spicy_storage):
    #parsing wtz & token in spicyswap storage

    if spicy_storage['token0']['fa2_address'] == 'KT1PnUZCp3u2KzWr93pn4DD7HAJnm3rWVrgn':
        sr0 = to_decimal(spicy_storage['reserve0'], 6)
        sr1 = to_decimal(spicy_storage['reserve1'], pairs[k]['decimal'])
        return sr0, sr1
    else:
        sr0 = to_decimal(spicy_storage['reserve1'], 6)
        sr1 = to_decimal(spicy_storage['reserve0'], pairs[k]['decimal'])
        return sr0, sr1

print('Started')
while True:
    #query mempool
    mempool = requests.get('https://eu01-node.teztools.net/chains/main/mempool/pending_operations').json()

    #for each applied transactions and with dexes entrypoint, add them to a contracts list
    for transactions in mempool['applied']:
        try:
            for contents in transactions['contents']:
                if contents['parameters']['entrypoint'] in quipu_entrypoints:
                    if contents['destination'] not in contracts:
                        contracts.append(contents['destination'])

                elif contents['parameters']['entrypoint'] in spicy_entrypoints:
                    if contents['parameters']['value']['args'][1]['args'][1]['args'][0]['string'] not in contracts:
                        contracts.append(e['parameters']['value']['args'][1]['args'][1]['args'][0]['string'])

        except Exception as error:
            pass

    #for each element in contracts list, analyze them if in pairs dict
    for k, v in pairs.items():
        if pairs[k]['spicy_address'] in contracts or pairs[k]['quipu_address'] in contracts:

            #get contracts storage
            spicy_storage = pairs[k]['spicy_pool'].storage()
            quipu_storage = pairs[k]['quipu_pool'].storage()

            #get token pooled
            #sr: spicy reserve
            #qr: quipu reserve
            spicy_r0, spicy_r1 = parse_spicy(spicy_storage) 
            quipu_r0 = to_decimal(quipu_storage['storage']['tez_pool'], 6)
            quipu_r1 = to_decimal(quipu_storage['storage']['token_pool'], pairs[k]['decimal'])

            #find solution for best arb from spicy to quipu
            solution = local_maxima(spicy_r0, spicy_r1, quipu_r1, quipu_r0)
            if solution:

                #print amount needed for spicyswap and the profit
                amount = formate_decimal(solution[0][0], 6)
                print(datetime.now(), f"Spicy to Quipu on {pairs[k]['name']}: {solution[0][1]} with {amount}")

            else:
                #find solution for best arb from quipu to spicy
                solution = local_maxima(quipu_r0, quipu_r1, spicy_r1, spicy_r0)
                if solution:

                    #print amount needed for spicyswap and the profit
                    amount = formate_decimal(solution[0][0], 6)
                    print(datetime.now(), f"Quipu to Spicy on {pairs[k]['name']}: {solution[0][1]} with {amount}")

    contracts = []
    time.sleep(28)
