def alarm_clock(day, vacation):
    if day in range(0, 4):
        if vacation:
            return '10:00'
        else:
            return '7:00'
    else:
        if not vacation:
            return '10:00'
        else:
            return 'OFF'

print(alarm_clock(1, False))
print(alarm_clock(5, False))
print(alarm_clock(0, True))
print(alarm_clock(6, True))

