
def E(n):
    numerator = 6**3 + (3*6**2)*n
    denominator = (6-n)*(6+n)**2
    return numerator, denominator, numerator/denominator

if __name__ == '__main__':
    for n in range(0,6):
        numerator, denominator, waitingTime = E(n)
        print('|$${0}$$|$$\\frac{{{1}}}{{{2}}}$$|$${3:.2f}$$|'.format(n, numerator, denominator, waitingTime))

