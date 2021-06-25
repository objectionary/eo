package org.eolang;

import org.eolang.hse.EObool;
import org.eolang.hse.EOfloat;
import org.eolang.hse.core.data.EODataObject;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Test cases for {@link EOfloat}
 */
class EOfloatTest {

    /***
     * Test for datization
     * Checks if the data is returned
     */
    @Test
    @DisplayName("Test Dataization")
    void _getData() {
        final EOfloat left = new EOfloat(12);
        MatcherAssert.assertThat(left._getData().toFloat(), Matchers.equalTo(12.0));
    }

    /***
     * Test for {@code EOadd}
     * checks if addition is successful
     */
    @Test
    void EOadd() {
        final EOfloat left = new EOfloat(12.0);
        final EOfloat right = new EOfloat(8.0);
        MatcherAssert.assertThat(
                left.EOadd(right)._getData().toFloat(),
                Matchers.equalTo(20.0)
        );
    }

    /***
     * Test for {@code EOsub}
     * checks if subtraction is successful
     */
    @Test
    void EOsub() {
        final EOfloat left = new EOfloat(12.0);
        final EOfloat right = new EOfloat(8.0);
        MatcherAssert.assertThat(
                left.EOsub(right)._getData().toFloat(),
                Matchers.equalTo(4.0)
        );
    }

    /***
     * Test for {@code EOdiv}
     * checks if division is successful
     */
    @Test
    void EOdiv() {
        final EOfloat left = new EOfloat(12.0);
        final EOfloat right = new EOfloat(4.0);
        MatcherAssert.assertThat(
                left.EOdiv(right)._getData().toFloat(),
                Matchers.equalTo(3.0)
        );
    }

    /***
     * Test for {@code EOmul}
     * checks if multiplication is successful
     */
    @Test
    void EOmul() {
        final EOfloat left = new EOfloat(12.0);
        final EOfloat right = new EOfloat(8.0);
        MatcherAssert.assertThat(
                left.EOmul(right)._getData().toFloat(),
                Matchers.equalTo(96.0)
        );
    }

    /***
     * Test for {@code EOeq}
     * checks equality (==)
     */
    @Test
    void EOeq() {
        final EOfloat left = new EOfloat(12.0);
        final EOfloat right = new EOfloat(12.0);
        final EObool eq = new EObool(left.EOeq(right)._getData().toBoolean());

        MatcherAssert.assertThat(
                eq._getData().toBoolean(),
                Matchers.equalTo(true)
        );
    }

    /***
     * Test for {@code EOneq}
     * checks equality (!=)
     */
    @Test
    void EOneq() {
        final EOfloat left = new EOfloat(12.0);
        final EOfloat right = new EOfloat(8.0);
        final EObool notEqual = new EObool(left.EOneq(right)._getData().toBoolean());

        MatcherAssert.assertThat(
                notEqual._getData().toBoolean(),
                Matchers.equalTo(true)
        );
    }

    /***
     * Test for {@code EOless}
     * checks if the left side value is less than that of the right side
     */
    @Test
    void EOless() {
        final EOfloat left = new EOfloat(4.0);
        final EOfloat right = new EOfloat(8.0);
        final EObool less = new EObool(left.EOless(right)._getData().toBoolean());

        MatcherAssert.assertThat(
                less._getData().toBoolean(),
                Matchers.equalTo(true)
        );
    }

    /***
     * Test for {@code EOleq}
     * checks if the left side value is less than or equal to that of the right side
     */
    @Test
    void EOleq() {
        final EOfloat left = new EOfloat(12.0);
        final EOfloat right = new EOfloat(12.0);
        final EObool lessThanOrEquals = new EObool(left.EOleq(right)._getData().toBoolean());

        MatcherAssert.assertThat(
                lessThanOrEquals._getData().toBoolean(),
                Matchers.equalTo(true)
        );
    }

    /***
     * Test for {@code EOgreater}
     * checks if the left side value is greater than that of the right side
     */
    @Test
    void EOgreater() {
        final EOfloat left = new EOfloat(12.0);
        final EOfloat right = new EOfloat(8.0);
        final EObool greater = new EObool(left.EOgreater(right)._getData().toBoolean());

        MatcherAssert.assertThat(
                greater._getData().toBoolean(),
                Matchers.equalTo(true)
        );
    }

    /***
     * Test for {@code EOgreater}
     * checks if the left side value is greater than or equal to that of the right side
     */
    @Test
    void EOgeq() {
        final EOfloat left = new EOfloat(12.0);
        final EOfloat right = new EOfloat(12.0);
        final EObool greaterOrEquals = new EObool(left.EOgeq(right)._getData().toBoolean());

        MatcherAssert.assertThat(
                greaterOrEquals._getData().toBoolean(),
                Matchers.equalTo(true)
        );
    }

    /***
     * Test for {@code EOneg}
     * checks if the base value gets negated
     */
    @Test
    void EOneg() {
        final EOfloat base = new EOfloat(12.0);
        final EOfloat neg = base.EOneg();

        MatcherAssert.assertThat(
                neg._getData().toFloat(),
                Matchers.equalTo(-12.0)
        );
    }

    /***
     * Test for {@code EOabs}
     * checks if the base value is returned as a non-negative number
     */
    @Test
    void EOabs() {
        final EOfloat left = new EOfloat(-12.0);
        final EOfloat absolute = left.EOneg();

        MatcherAssert.assertThat(
                absolute._getData().toFloat(),
                Matchers.equalTo(12.0)
        );
    }

    /**
     * Tests for all three possibilities of {@code signum}
     * checks if the correct sign value is returned
     *
     * @param number an integer representing the test value to apply {@code EOsignum} to
     */
    @ParameterizedTest(name = "{0}")
    @ValueSource(doubles = {-23.0, 0.0, 7.0})
    @DisplayName("Test signum")
    void EOsignum(double number) {
        MatcherAssert.assertThat(
                new EOfloat(
                        number
                ).EOsignum()._getData().toFloat(),
                Matchers.equalTo(Math.signum(number))
        );
    }

    /***
     * Test for {@code EOpow}
     * checks if a number raised to a power is correctly evaluated
     * @param exponent An integer representing the exponent
     */
    @ParameterizedTest(name = "{0}")
    @ValueSource(doubles = {-1.0, 0.0, 1.0, 2.0, 3.0})
    @DisplayName("Test powers")
    void EOpow(double exponent) {
        MatcherAssert.assertThat(
                new EOfloat(
                        0.0
                ).EOpow(
                        new EODataObject(
                                exponent
                        )
                )._getData().toFloat(),
                Matchers.equalTo(Math.pow(0.0, exponent))
        );
    }
}