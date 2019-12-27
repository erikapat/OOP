


def foo(n):
    from past.builtins import xrange
    phrase = 'repeat me'
    pmul = phrase * n
    pjoi = ''.join([phrase for x in xrange(n)])
    pinc = ''
    for x in xrange(n):
        pinc += phrase
    del pmul, pjoi, pinc