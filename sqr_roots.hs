resfn a b c | a /= 0 = (((-b) + sqrt(b*b - 4*a*c))/(2*a), ((-b) - sqrt(b*b - 4*a*c))/(2*a))
            | a == 0 && b /= 0 = ((-c) / b, (-c) / b)
            | a == 0 && b == 0 && c /= 0 = error "A and B can't be null"
            | a == 0 && b == 0 && c == 0 = error " Infinity"
