#include "fdlibm.h"
#include <math.h>
#include <stdlib.h>


typedef struct {
	double eqt;
	double decl;
} SunPos;

double rad2deg(double);
double deg2rad(double);
double darctan2(double, double);
double darcsin(double);
double dcos(double);
double dsin(double);
double fix_hour(double);
double fix_angle(double);
double sun_position_decl(double);
double sun_position_eqt(double);
SunPos sun_position(double);
double get_julian_date(int, int, int);

double sun_position_decl_gd(int year, int month, int day) {
	return sun_position_decl(get_julian_date(year, month, day));
}
double sun_position_eqt_gd(int year, int month, int day) {
	return sun_position_eqt(get_julian_date(year, month, day));
}

	double get_julian_date(int year, int month, int day)
	{
		if (month <= 2)
		{
			year -= 1;
			month += 12;
		}

		double a = floor(year / 100.0);
		double b = 2 - a + floor(a / 4.0);

		return floor(365.25 * (year + 4716)) + floor(30.6001 * (month + 1)) + day + b - 1524.5;
	}


double sun_position_decl(double jd)
	{
		return sun_position(jd).decl;
	}

double sun_position_eqt(double jd)
	{
		return sun_position(jd).eqt;
	}

double d(double jd) {
	return jd - 2451545.0;
}

double fg(double dD) {
	return fix_angle(357.5291 + 0.98560028 * dD);
}

double fq(double dD) {
		return fix_angle(280.459 + 0.98564736 * dD);
}

double Lterm1(double q, double g) {
		return q;
}

double Lterm2(double q, double g) {
		return 1.9148 * dsin(g);
}

double Lterm3(double q, double g) {
		return 0.020 * dsin(2 * g);
}

double L(double q, double g) {
		return fix_angle(Lterm1(q, g) + Lterm2(q, g) + Lterm3(q, g));
}

double fe(double dD) {
		return 23.4393 - 0.00000036 * dD;
}

double pi() {
	return M_PI;
}


SunPos sun_position(double jd)
	{
		double dD = d(jd);
		double g = fg(dD);
		double q = fq(dD);
		double l = L(q, g);

		// double r = 1.00014 - 0.01671 * dcos(g) - 0.00014 * dcos(2 * g);
		double e = fe(dD);

		double dd = darcsin(dsin(e) * dsin(l));
		double ra = darctan2(dcos(e) * dsin(l), dcos(l)) / 15.0;
		ra = fix_hour(ra);
		double eq_t = q / 15.0 - ra;

		SunPos pos;
		pos.decl = dd;
		pos.eqt = eq_t;

		return pos;
	}

	/* range reduce angle in degrees. */
	double fix_angle(double a)
	{
		while (a >= 360) a -= 360;
		while (a < 0) a += 360;
		return a;
	}

	/* range reduce hours to 0..23 */
	double fix_hour(double a)
	{
		while (a >= 24) a -= 24;
		while (a < 0) a += 24;
		return a;
	}

	/* degree sin */
	double dsin(double d)
	{
		return sin_fdlibm(deg2rad(d));
	}

	/* degree cos */
	double dcos(double d)
	{
		return cos_fdlibm(deg2rad(d));
	}

	/* degree tan */
	double dtan(double d)
	{
		return tan_fdlibm(deg2rad(d));
	}

	/* degree arcsin */
	double darcsin(double x)
	{
		return rad2deg(asin_fdlibm(x));
	}

	/* degree arccos */
	double darccos(double x)
	{
		return rad2deg(acos_fdlibm(x));
	}

	/* degree arctan */
	double darctan(double x)
	{
		return rad2deg(atan_fdlibm(x));
	}

	/* degree arctan2 */
	double darctan2(double y, double x)
	{
		return rad2deg(atan2_fdlibm(y, x));
	}
	/* degree arccot */
	double darccot(double x)
	{
		return rad2deg(atan_fdlibm(1.0 / x));
	}

	/* degree to radian */
	double deg2rad(double d)
	{
		return d * M_PI / 180.0;
	}

	/* radian to degree */
	double rad2deg(double r)
	{
		return r * 180.0 / M_PI;
	}

    double computeAsr(double step, double D, double lat) {
        double G = -darccot(step + dtan(fabs(lat - D)));
        return G;
    }
