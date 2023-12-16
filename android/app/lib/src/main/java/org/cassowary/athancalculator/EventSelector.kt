package org.cassowary.athancalculator

sealed class EventSelector
data class SpecificAngle(val degrees: Double): EventSelector()
data class FixedOffset (val seconds: Long): EventSelector()