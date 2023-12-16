package org.cassowary.athancalculator

import java.lang.Double.isNaN
import java.time.Duration
import java.time.LocalDate
import java.time.temporal.ChronoUnit

fun sunPosition(day: LocalDate): SunPosition {
    val small_d = calc_small_d(day)

    val g = calc_g(small_d);
    val q = calc_q(small_d);
    val LL = calc_LL(q, g);

    val e = calc_e(small_d);
    val D = arcsine_degrees(sin_degrees(e) * sin_degrees(LL))

    val RRA_timeDegrees = arctangent2_degrees (cosine_degrees(e) * sin_degrees(LL), cosine_degrees(LL));
    val RRA_hours = angleToDuration(RRA_timeDegrees);
    val equationOfTime = angleToDuration(q) - RRA_hours;

    return SunPosition(equationOfTime, declinationAngleDegrees = D);
}

fun angleToDuration(degrees: Double): Double {
    return fix_hours(degrees/15.0)
}

private fun bound_amount(value: Double, boundary: Double): Double {
    var my_value = value;
    while (my_value > boundary) my_value -= boundary
    while (my_value < 0) my_value += boundary
    return my_value;
}

private fun fix_degrees(degrees: Double): Double {
    return bound_amount(degrees, 360.0);
}

private fun fix_hours(hours: Double): Double {
    return bound_amount(hours, 24.0);
}

fun calc_small_d(day: LocalDate): Double {
    return to_julian_date(day) - 2451545.0;
}

fun to_julian_date(day: LocalDate): Double {
    return 2400000.5 + to_modified_julian_date(day)
}

private fun to_modified_julian_date(day: LocalDate): Long {
    val epoch = LocalDate.of(1858, 11, 17);
    return ChronoUnit.DAYS.between(epoch, day)
}


fun calc_g(small_d: Double): Double {
    return fix_degrees(357.5291 + 0.98560028 * small_d);
}

fun calc_q(small_d: Double): Double {
    return fix_degrees(280.459 + 0.98564736 * small_d);

}

fun calc_LL(q: Double, g: Double): Double {
    return fix_degrees(q + 1.9148 * (sin_degrees(g)) + (0.020 * sin_degrees(2*g)))
}

fun calc_e(small_d: Double): Double {
    return 23.4393 - (0.00000036 * small_d)
}

data class SunPosition(val equationOfTime: Double, val declinationAngleDegrees: Double) {
    fun noon(): Duration {
        return Duration.ofHours(12).minus(Duration.ofSeconds(extra_noon_secs()))
       // return Duration.ofHours(12).seconds - equationOfTimeSeconds; // todo does this work the way i want?
    }

    fun extra_noon_secs(): Long {
        return extra_noon().toLong()
    }

    fun extra_noon(): Double {
        return 60 * 60 * equationOfTime
    }

    fun solarAngleOffset(location: Location, alpha: Double): Duration? {
        val declination = this.declinationAngleDegrees
        val top = - sin_degrees(alpha) - sin_degrees(location.latitude) * sin_degrees(declination)
        val bottom = cosine_degrees(location.latitude) * cosine_degrees(declination)
        val timeAngle = arccos_degrees(top / bottom);
        if(isNaN(timeAngle))
            return null;
        return Duration.ofSeconds((60*60*angleToDuration(timeAngle)).toLong());
    }
}
