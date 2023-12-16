package org.cassowary.athancalculator

// we don't want to use Math.toRadians because the 
// order of operations changes the result
fun toRadians(degrees: Double): Double {
	return degrees * Math.PI / 180;
}

fun toDegrees(radians: Double): Double {
    return radians * 180 / Math.PI;
}

fun sin_degrees(degrees: Double): Double {
    return StrictMath.sin(toRadians(degrees));
}

fun arcsine_degrees(num: Double): Double {
    return toDegrees(StrictMath.asin(num));
}

fun cosine_degrees(degrees: Double): Double {
    return StrictMath.cos(toRadians(degrees))
}

fun arccos_degrees(num: Double): Double {
    return toDegrees(StrictMath.acos(num));
}

fun tangent_degrees(degrees: Double): Double {
    return StrictMath.tan(toRadians(degrees))
}

fun arctangent2_degrees(x: Double, y: Double): Double {
    return toDegrees(StrictMath.atan2(x, y));
}

fun arctangent_degrees(num: Double): Double {
    return toDegrees(StrictMath.atan(num));
}

fun arccotangent_degrees(num: Double): Double {
    return arctangent_degrees(1.0/num)
}
