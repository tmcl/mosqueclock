package org.cassowary.athancalculator

import java.time.ZoneOffset

data class Location(val longitude: Double, val latitude: Double) {
    companion object {
        fun hamburg(): Location {
            return Location(9.99999914413, 53.5500246369)
        }


    fun naturalTimeZone(longitude: Double): ZoneOffset {
        return ZoneOffset.ofTotalSeconds(60*(60.0*longitude/15.0).toInt())
    }
    }

}