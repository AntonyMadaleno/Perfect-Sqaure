import itertools

def dupExist(lst):
    def dupExistAux(lst, b):
        if not lst:
            return False
        hd, *tl = lst
        if hd in b:
            return True
        else:
            return dupExistAux(tl, [hd] + b)
    return dupExistAux(lst, [])

def find_subdivisions(n, curr_size, curr_sum, acc):
    if curr_sum == n*n:
        return list(reversed(acc))
    elif curr_sum > n*n:
        return None
    else:
        next_size = curr_size + 1
        max_size = n - (n % curr_size)
        def try_sizes(s):
            if s > max_size:
                return None
            else:
                new_sum = curr_sum + s*s
                res = find_subdivisions(n, next_size, new_sum, [s] + acc)
                if res is not None:
                    return res
                else:
                    return try_sizes(s+1)
        return try_sizes(curr_size)

def find_smallest_combination(n, curr_size, curr_sum, curr_acc, smallest_combination = None):
    if curr_size >= n:
        return smallest_combination or []
    else:
        res = find_subdivisions(n, curr_size, curr_sum, curr_acc)
        if res is not None:
            new_smallest_combination = smallest_combination
            if smallest_combination is None:
                new_smallest_combination = res
            elif len(res) < 2:
                pass
            elif dupExist(res):
                pass
            elif len(res) < len(smallest_combination):
                new_smallest_combination = res
            smallest_combination = new_smallest_combination
        return find_smallest_combination(n, curr_size+1, curr_sum, curr_acc, smallest_combination)

def is_subdivisable(n):
    return find_subdivisions(n, 1, 0, []) != None


def init(n , s, n_plate = 3):

    p = [[n],[1]]

    for e in s:
        p[0].append(e)
        p[1].append(e)

    p[0].append(n)
    p[1].append(1)

    return p

def haveSolution(p):

    for i in range ( 1, len(p[0]) - 1 ):

        if p[1][i] < p[0][i-1] - p[0][i] and p[1][i] < p[0][i+1] - p[0][i]:
            return False

    return True



def algo(n, n_plate = 3):

    if(is_subdivisable(n)):

        p = None
        li = []

        for x in range( 1, n-(2*n_plate) ):
            li.append(x)

        for st in itertools.combinations(li, n_plate): 


                    if sum(st) == n and len(set(st)) == len(st):
                        
                        order = []

                        p = init(n, st, n_plate)

                        if haveSolution(p):
                            #si il est possible d'avoir une solution

                            for x in st:
                                order.append(x)

                            def try_horizontal_ext(p, n_plate, order):

                                cond = False

                                for t in range(1, n_plate + 1):

                                    delta = [ p[0][t-1] - p[0][t] , p[0][t+1] - p[0][t] ]

                                    if delta[0] == p[1][t] and delta[0] + p[0][t] == sum(st):
                                        if vertical_ext(p, t, order):
                                            cond = True
                                            if verif(n, order, p):
                                                return order

                                    if delta[1] == p[1][t] and delta[1] + p[0][t] == sum(st):
                                        if vertical_ext(p, t, order):
                                            cond = True
                                            if verif(n, order, p):
                                                return order

                                    if t != n_plate and delta[1] > 0:
                                        if horizontal_ext(p, t, delta[1], order, 1):
                                            cond = True
                                            if verif(n, order, p):
                                                return order

                                    if t != 1 and delta[0] > 0:
                                        if horizontal_ext(p, t, delta[0], order, -1):
                                            cond == True
                                            if verif(n, order, p):
                                                return order

                                if verif(n, order, p):
                                    return order

                                if cond:
                                    return try_horizontal_ext(p, n_plate, order)
                                else:
                                    return try_vertical_ext(p, n_plate, order)

                            def try_vertical_ext(p, n_plate, order):

                                indices = [i[0] for i in sorted(enumerate(p[1]), key=lambda x:x[1])]
                                indices.remove(0)
                                indices.remove( len(p[0])-1 )

                                for t in indices:

                                    if vertical_ext(p, t, order):
                                        return try_horizontal_ext(p, n_plate, order)

                                return order

                            order = try_horizontal_ext(p, n_plate, order)

                            if (n == p[1][1] + p[1][2] + p[1][3] and verif(n, order, p)):
                                return order

    else:
        print(f"No solution found for value {n} !")
        return None

def verif(n, order, p):

    if not ( n * len(p[0]) == sum(p[0]) ):
        return False

    add = 0
    for x in order:
        add += x*x

    return add == n*n

def vertical_ext(p, i, order):
    v = p[1][i]

    if (v in order):
        return False

    p_old = []
    p_old.append(p[0].copy())
    p_old.append(p[1].copy())

    order.append(v)

    p[0][i] += v

    for x in range (1, len(p[0]) - 1):

        if p[1][i] < p[0][i-1] - p[0][i] and p[1][i] < p[0][i+1] - p[0][i]:
            p[0] = p_old[0].copy()
            p[1] = p_old[1].copy()
            order.pop()
            return False

    return True

def horizontal_ext(p, i, v, order, d):

    if (v in order or v > p[1][i]):
        return False

    p_old = []
    p_old.append(p[0].copy())
    p_old.append(p[1].copy())

    order.append(v)

    if ( not (d == -1 and i == 1) ) and not ( d == 1 and i == len(p[0]) - 2 ):
        p[1][i+d]   += v
        p[1][i]     -= v

    for x in range (1, len(p[0]) - 1):

        if p[1][i] < p[0][i-1] - p[0][i] and p[1][i] < p[0][i+1] - p[0][i]:
            p[0] = p_old[0].copy()
            p[1] = p_old[1].copy()
            order.pop()
            return False

    return True

res = algo(112, 3)
print( f"Square Placed In order : {res}\tN = 112\t N_BASE = 3\n" )

res = algo(672, 3)
print( f"Square Placed In order : {res}\tN = 2462\t N_BASE = 3\n" )
