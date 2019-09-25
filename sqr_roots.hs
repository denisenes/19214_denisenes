resfn 0 0 0 = error " Infinity"
resfn 0 0 c = error "A and B can't be null"
resfn 0 b c = ((-c) / b, (-c) / b)
resfn a b c = (((-b) + disc)/(2*a), ((-b) - disc)/(2*a)) where disc = sqrt(b*b - 4*a*c)
