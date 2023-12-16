package org.cassowary.athancalculator

import java.time.*
import java.time.temporal.ChronoField
import kotlin.math.abs
import kotlin.math.roundToInt

class TimeComputer(val method: Method, val location: Location, val localDate: LocalDate) {
    fun computeTimes(): Athans<ZonedDateTime> {
        val spos = sunPosition(localDate)
        val sunSizeAngle = 5.0/6.0

        // default times: (DST = that day's noon anchored solar time
        //   - if asr is not defined, we pick 3pm or 4pm DST depending on the Asr rule
        //   - if sunrise or sunset are undefined, we pick 6am or 6pm DST
        //   - if fajr is undefined according to its angle, we pick midnight (DST)
        //   - if isha is undefined, it is ten minutes before fajr.

        val noon = spos.noon()

        val fajr = beforeNoon(method.fajrAngle, spos) ?: noon - Duration.ofHours(12);
        val sunrise = beforeNoon(sunSizeAngle, spos) ?: noon - Duration.ofHours(6);
        val thuhr = noon + Duration.ofSeconds(method.thuhrSetting.seconds)
        val asr = afterNoon(calc_asr(spos, method.asrSetting), spos)
        val defaultMaghrib = noon + Duration.ofHours(6)
        val maghrib = delays(afterNoon(sunSizeAngle, spos) ?: defaultMaghrib, spos, method.maghribSetting) ?: defaultMaghrib
        val isha = delay(maghrib, spos, method.ishaSetting) ?: fajr - Duration.ofMinutes(10) + Duration.ofHours(24)

        return Athans(
            roundedLaterZonedTime(fajr ?: Duration.ZERO),
            roundedEarlierZonedTime(sunrise),
            roundedLaterZonedTime(thuhr),
            roundedLaterZonedTime(asr ?: noon + Duration.ofHours(if (method.asrSetting == AsrRule.Earlier) 3 else 4)),
            roundedLaterZonedTime(maghrib),
            roundedLaterZonedTime(isha)
        );
    }

    private fun delay(duration: Duration, spos: SunPosition, eventSelector: EventSelector): Duration? {
        return when (eventSelector) {
            is SpecificAngle -> afterNoon(eventSelector.degrees, spos)
            is FixedOffset -> duration + Duration.ofSeconds(eventSelector.seconds)
        }
    }

    private fun delays(duration: Duration, spos: SunPosition, eventSelectors: List<EventSelector>): Duration? {
        var dur: Duration = duration
        eventSelectors.forEach {
            dur = delay(dur, spos, it) ?: return null
        }
        return dur
    }

    // note: to strictly be equal to our haskell implementation, this should be
    // 60 * (60.0 * location.longitude/15.0).roundToInt()
    private val naturalTimezone: ZoneOffset
    // = ZoneOffset.ofTotalSeconds(0)
     = Location.naturalTimeZone(location.longitude)

    private fun roundedEarlierZonedTime(fromMidnight: Duration): ZonedDateTime {
        return zonedTime(fromMidnight)
            .withSecond(0)
            .withNano(0)
    }

    private fun roundedLaterZonedTime(fromMidnight: Duration): ZonedDateTime {
        return zonedTime(fromMidnight)
            .withNano(0)
            .plusSeconds(59)
            .withSecond(0)
    }

    private fun zonedTime(fromMidnight: Duration): ZonedDateTime {
        return localDate.atStartOfDay().plus(fromMidnight).atZone(naturalTimezone)
    }

    private fun beforeNoon(alpha: Double, spos: SunPosition): Duration? {
        val solarAngle = spos.solarAngleOffset(location, alpha) ?: return null
        return spos.noon() - solarAngle
    }

    private fun afterNoon(alpha: Double, spos: SunPosition): Duration? {
        val solarAngle = spos.solarAngleOffset(location, alpha) ?: return null
        return spos.noon() + solarAngle
    }

    private fun calc_asr(spos: SunPosition, asrRule: AsrRule): Double {
        val size = if(asrRule == AsrRule.Earlier) 1 else 2;
        val decl = spos.declinationAngleDegrees
        return -arccotangent_degrees(size + tangent_degrees(abs(location.latitude - decl)))
    }
}

