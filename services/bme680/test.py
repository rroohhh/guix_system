#!/usr/bin/env python3

import bme680, time, display

disp = display.open()
disp.clear()
disp.backlight(0)
disp.close()

with bme680.Bme680() as environment:
    while True:
        d = environment.get_data()

        print(
            "indoor_air_quality,location=robin_room temperature={},humidity={},pressure={},gas_resistance={},iaq={},iaq_accuracy={},eco2={}".format(
            d.temperature,
            d.humidity,
            d.pressure,
            d.gas_resistance,
            d.iaq,
            d.iaq_accuracy,
            d.eco2
        ))

        time.sleep(10)
