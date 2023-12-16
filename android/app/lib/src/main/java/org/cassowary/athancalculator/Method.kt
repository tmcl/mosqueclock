package org.cassowary.athancalculator


data class Method(
    val fajrAngle: Double,
    val thuhrSetting: ThuhrSetting,
    val asrSetting: AsrRule,
    val maghribSetting: List<EventSelector>,
    val ishaSetting: EventSelector
) {
    companion object {
        fun ours(): Method {
            return Method(
                15.0,
                ThuhrSetting.Instant,
                AsrRule.Earlier,
                listOf(SpecificAngle(5.0/6.0), FixedOffset(30)),
                SpecificAngle(17.0)
            )
        }
    }
}

