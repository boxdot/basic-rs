use std::iter;
use std::str;

// FIXME: Refactor using structs and float manipulation and not string manipulation as below.
// I also strongly believe, that the implementation below is *slow*.
pub fn format_float(value: f64) -> String {
    const SIGNIFICANCE_WIDTH: usize = 6;
    // const EXTRAD_WIDTH: usize = 2;

    if value == 0.0 {
        return " 0 ".into();
    } else if value.is_nan() {
        return " NAN ".into();
    } else if value.is_infinite() {
        if value.is_sign_positive() {
            return " INF ".into();
        } else {
            return "-INF ".into();
        }
    }

    let sign_str = if value.is_sign_positive() { ' ' } else { '-' };

    let value_str = format!("{}", value.abs());
    let mut parts = value_str.split('.');
    let integ = parts.next().unwrap();
    let integ = if integ == "0" { "" } else { integ };
    let fract = parts.next().unwrap_or("");

    if integ.len() + fract.len() > SIGNIFICANCE_WIDTH {
        // explicit point scaled notation
        let value_str = format!("{:.width$E}", value.abs(), width = SIGNIFICANCE_WIDTH - 1);
        let mut parts = value_str.split('E');
        let significant = parts.next().unwrap();
        let extrad = parts.next().unwrap();
        let extrad_sign = if extrad.chars().nth(0).unwrap() != '-' {
            "+"
        } else {
            ""
        };
        let mut parts = significant.split('.');
        let integ = parts.next().unwrap();
        let fract = parts.next().unwrap();

        let fract = fract.trim_right_matches('0');

        let extrad_int: i64 = str::parse(extrad).unwrap();
        if extrad_int > 0
            && integ.len() + fract.len().max(extrad_int.abs() as usize) <= SIGNIFICANCE_WIDTH
        {
            // special case: we shall omit E by moving comma to the right
            let n_shift = extrad_int.abs() as usize;
            let integ: String = integ
                .chars()
                .chain(fract.chars().chain(iter::repeat('0')).take(n_shift))
                .collect();
            let fract: String = fract.chars().skip(n_shift).collect();
            let full_stop = if !fract.is_empty() { "." } else { "" };
            format!("{}{}{}{} ", sign_str, integ, full_stop, fract)
        } else if extrad_int < 0
            && integ.len().max(extrad_int.abs() as usize) + fract.len() <= SIGNIFICANCE_WIDTH
        {
            // special case: we shall omit E by moving comma to the left
            let n_shift = extrad_int.abs() as usize;
            let fract: String = iter::repeat('0')
                .take(n_shift.saturating_sub(integ.len()))
                .chain(
                    integ
                        .chars()
                        .skip(integ.len().saturating_sub(n_shift))
                        .chain(fract.chars()),
                ).collect();
            let integ = &integ[..integ.len().saturating_sub(n_shift)];
            let full_stop = if !fract.is_empty() { "." } else { "" };
            format!("{}{}{}{} ", sign_str, integ, full_stop, fract)
        } else if extrad_int == 0 {
            let full_stop = if !fract.is_empty() { "." } else { "" };
            format!("{}{}{}{} ", sign_str, integ, full_stop, fract)
        } else {
            format!("{}{}.{}E{}{} ", sign_str, integ, fract, extrad_sign, extrad)
        }
    } else {
        // implicit or explicit unscaled point notation
        let full_stop = if !fract.is_empty() { "." } else { "" };
        format!("{}{}{}{} ", sign_str, integ, full_stop, fract)
    }
}

#[cfg(test)]
mod tests {
    use super::format_float;

    #[test]
    fn format_zero() {
        assert_eq!(&format_float(0.0), " 0 ");
        assert_eq!(&format_float(-0.0), " 0 ");
    }

    #[test]
    fn format_implicit_point_unscaled_notation() {
        assert_eq!(&format_float(1.0), " 1 ");
        assert_eq!(&format_float(12.0), " 12 ");
        assert_eq!(&format_float(123.0), " 123 ");
        assert_eq!(&format_float(1234.0), " 1234 ");
        assert_eq!(&format_float(12345.0), " 12345 ");
        assert_eq!(&format_float(123456.0), " 123456 ");

        assert_eq!(&format_float(-1.0), "-1 ");
        assert_eq!(&format_float(-12.0), "-12 ");
        assert_eq!(&format_float(-123.0), "-123 ");
        assert_eq!(&format_float(-1234.0), "-1234 ");
        assert_eq!(&format_float(-12345.0), "-12345 ");
        assert_eq!(&format_float(-123456.0), "-123456 ");
    }

    #[test]
    fn format_explicit_point_unscaled_notation() {
        assert_eq!(&format_float(0.1), " .1 ");
        assert_eq!(&format_float(0.12), " .12 ");
        assert_eq!(&format_float(0.123), " .123 ");
        assert_eq!(&format_float(0.1234), " .1234 ");
        assert_eq!(&format_float(0.12345), " .12345 ");
        assert_eq!(&format_float(0.123456), " .123456 ");
        assert_eq!(&format_float(0.234567), " .234567 ");
        assert_eq!(&format_float(0.345678), " .345678 ");
        assert_eq!(&format_float(0.456789), " .456789 ");
        assert_eq!(&format_float(0.56789), " .56789 ");
        assert_eq!(&format_float(0.6789), " .6789 ");
        assert_eq!(&format_float(0.789), " .789 ");
        assert_eq!(&format_float(0.89), " .89 ");
        assert_eq!(&format_float(0.9), " .9 ");

        assert_eq!(&format_float(-0.1), "-.1 ");
        assert_eq!(&format_float(-0.12), "-.12 ");
        assert_eq!(&format_float(-0.123), "-.123 ");
        assert_eq!(&format_float(-0.1234), "-.1234 ");
        assert_eq!(&format_float(-0.12345), "-.12345 ");
        assert_eq!(&format_float(-0.123456), "-.123456 ");
        assert_eq!(&format_float(-0.234567), "-.234567 ");
        assert_eq!(&format_float(-0.345678), "-.345678 ");
        assert_eq!(&format_float(-0.456789), "-.456789 ");
        assert_eq!(&format_float(-0.56789), "-.56789 ");
        assert_eq!(&format_float(-0.6789), "-.6789 ");
        assert_eq!(&format_float(-0.789), "-.789 ");
        assert_eq!(&format_float(-0.89), "-.89 ");
        assert_eq!(&format_float(-0.9), "-.9 ");

        assert_eq!(&format_float(1.23456), " 1.23456 ");
        assert_eq!(&format_float(9.876), " 9.876 ");
        assert_eq!(&format_float(12.3456), " 12.3456 ");
        assert_eq!(&format_float(123.456), " 123.456 ");
        assert_eq!(&format_float(1234.56), " 1234.56 ");
        assert_eq!(&format_float(12345.6), " 12345.6 ");
        assert_eq!(&format_float(23456.7), " 23456.7 ");
        assert_eq!(&format_float(34567.8), " 34567.8 ");
        assert_eq!(&format_float(45678.9), " 45678.9 ");
        assert_eq!(&format_float(56789.1), " 56789.1 ");
        assert_eq!(&format_float(67891.2), " 67891.2 ");
        assert_eq!(&format_float(78912.3), " 78912.3 ");
        assert_eq!(&format_float(89123.4), " 89123.4 ");
        assert_eq!(&format_float(91234.5), " 91234.5 ");
        assert_eq!(&format_float(99999.9), " 99999.9 ");

        assert_eq!(&format_float(-1.23456), "-1.23456 ");
        assert_eq!(&format_float(-9.876), "-9.876 ");
        assert_eq!(&format_float(-12.3456), "-12.3456 ");
        assert_eq!(&format_float(-123.456), "-123.456 ");
        assert_eq!(&format_float(-1234.56), "-1234.56 ");
        assert_eq!(&format_float(-12345.6), "-12345.6 ");
        assert_eq!(&format_float(-23456.7), "-23456.7 ");
        assert_eq!(&format_float(-34567.8), "-34567.8 ");
        assert_eq!(&format_float(-45678.9), "-45678.9 ");
        assert_eq!(&format_float(-56789.1), "-56789.1 ");
        assert_eq!(&format_float(-67891.2), "-67891.2 ");
        assert_eq!(&format_float(-78912.3), "-78912.3 ");
        assert_eq!(&format_float(-89123.4), "-89123.4 ");
        assert_eq!(&format_float(-91234.5), "-91234.5 ");
        assert_eq!(&format_float(-99999.9), "-99999.9 ");
    }

    #[test]
    fn format_explicit_point_scaled_notation() {
        assert_eq!(&format_float(0.0000001), " 1.E-7 ");
        assert_eq!(&format_float(-0.0000001), "-1.E-7 ");

        assert_eq!(&format_float(123456E27), " 1.23456E+32 ");
        assert_eq!(&format_float(123456E+27), " 1.23456E+32 ");
        assert_eq!(&format_float(123456.0E27), " 1.23456E+32 ");
        assert_eq!(&format_float(123456.0E+27), " 1.23456E+32 ");
        assert_eq!(&format_float(12345.6E28), " 1.23456E+32 ");
        assert_eq!(&format_float(12345.6E+28), " 1.23456E+32 ");
        assert_eq!(&format_float(1234.56E29), " 1.23456E+32 ");
        assert_eq!(&format_float(1234.56E+29), " 1.23456E+32 ");
        assert_eq!(&format_float(123.456E30), " 1.23456E+32 ");
        assert_eq!(&format_float(123.456E+30), " 1.23456E+32 ");
        assert_eq!(&format_float(12.3456E31), " 1.23456E+32 ");
        assert_eq!(&format_float(12.3456E+31), " 1.23456E+32 ");
        assert_eq!(&format_float(1.23456E32), " 1.23456E+32 ");
        assert_eq!(&format_float(1.23456E+32), " 1.23456E+32 ");
        assert_eq!(&format_float(0.123456E33), " 1.23456E+32 ");
        assert_eq!(&format_float(0.123456E+33), " 1.23456E+32 ");
        assert_eq!(&format_float(1.2345600000000E32), " 1.23456E+32 ");
        assert_eq!(&format_float(1.2345600000000E+32), " 1.23456E+32 ");
        assert_eq!(&format_float(0.00000123456E38), " 1.23456E+32 ");
        assert_eq!(&format_float(0.00000123456E+38), " 1.23456E+32 ");
        assert_eq!(&format_float(0.00001234560000E37), " 1.23456E+32 ");
        assert_eq!(&format_float(0.00001234560000E+37), " 1.23456E+32 ");

        assert_eq!(&format_float(123456E-29), " 1.23456E-24 ");
        assert_eq!(&format_float(123456.0E-29), " 1.23456E-24 ");
        assert_eq!(&format_float(12345.6E-28), " 1.23456E-24 ");
        assert_eq!(&format_float(1234.56E-27), " 1.23456E-24 ");
        assert_eq!(&format_float(123.456E-26), " 1.23456E-24 ");
        assert_eq!(&format_float(12.3456E-25), " 1.23456E-24 ");
        assert_eq!(&format_float(1.23456E-24), " 1.23456E-24 ");
        assert_eq!(&format_float(0.123456E-23), " 1.23456E-24 ");
        assert_eq!(&format_float(1.2345600000000E-24), " 1.23456E-24 ");
        assert_eq!(&format_float(0.00000123456E-18), " 1.23456E-24 ");
        assert_eq!(&format_float(0.00001234560000E-19), " 1.23456E-24 ");
    }

    #[test]
    fn test_omitting_extrand() {
        assert_eq!(&format_float(4.56E+1), " 45.6 ");
        assert_eq!(&format_float(4.56E-1), " .456 ");
        assert_eq!(&format_float(45.6E-1), " 4.56 ");
    }

    #[test]
    fn test_round_explicit_point_notation() {
        assert_eq!(&format_float(1234567886.0), " 1.23457E+9 ");
        assert_eq!(&format_float(9.999999999), " 10 ");
    }
}
