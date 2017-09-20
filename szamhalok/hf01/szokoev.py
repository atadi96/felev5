def szokoev(ev):
    return ev%400 == 0 or (ev%4 == 0 and ev%100 != 0)

print(szokoev(2016))
print(szokoev(2017))
