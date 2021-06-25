package org.eolang.hse;

import org.eolang.hse.core.EOObject;
import org.eolang.hse.core.data.EOData;

/***
 * Represents a float number
 * @version %I% %G%
 */

public class EOfloat extends EOObject {
    private final double value;

    public EOfloat(double value) {
        this.value = value;
    }

    @Override
    public EOData _getData() {
        return new EOData(value);
    }

    /***
     * Adds the {@code rightAddend} free attribute to this float
     * @param rightAddend a number to be added to this float
     * @return An object representing the sum of this float and the {@code rightAddend} free attribute
     */
    public EOfloat EOadd(EOObject rightAddend) {
        return new EOfloat(this.value + rightAddend._getData().toFloat());
    }

    /***
     * Subtracts the {@code subtrahend} free attribute from this float
     * @param subtrahend a number to be subtracted from this float
     * @return An object representing the difference between of this float and the {@code subtrahend} free attribute
     */
    public EOfloat EOsub(EOObject subtrahend) {
        return new EOfloat(this.value - subtrahend._getData().toFloat());
    }

    /***
     * Divides this float by the {@code divisor} free attribute
     * @param divisor a number which divides this float
     * @return An object representing the quotient of this float divided by the {@code divisor} free attribute
     */
    public EOfloat EOdiv(EOObject divisor) {
        if (divisor._getData().toFloat() == 0.0) {
            throw new ArithmeticException("Division by zero not possible");
        }
        return new EOfloat(this.value / divisor._getData().toFloat());
    }

    /***
     * Multiplies this float by the {@code multiplier} free attribute
     * @param multiplier a number by which this float is to be multiplied
     * @return An object representing the product of this float and the {@code multiplier} free attribute
     */
    public EOfloat EOmul(EOObject multiplier) {
        return new EOfloat(this.value * multiplier._getData().toFloat());
    }

    /***
     * Checks if this float is equal to the {@code rightFloat} free attribute
     * @param  rightFloat a number to compare this float to
     * @return A boolean representing the truth value of the comparison of this float with the {@code rightFloat} free attribute
     */
    public EObool EOeq(EOObject rightFloat) {
        return new EObool(this.value == rightFloat._getData().toFloat());
    }

    /***
     * Checks if this float is not equal to the {@code rightFloat} free attribute
     * @param  rightFloat a number to compare this float to
     * @return A boolean representing the truth value of the comparison of this float with the {@code rightFloat} free attribute
     */
    public EObool EOneq(EOObject rightFloat) {
        return new EObool(this.value != rightFloat._getData().toFloat());
    }

    /***
     * Checks if this float is less than the {@code rightFloat} free attribute
     * @param  rightFloat a number to compare this float to
     * @return A boolean representing the truth value of the comparison of this float with the {@code rightFloat} free attribute
     */
    public EObool EOless(EOObject rightFloat) {
        return new EObool(this.value < rightFloat._getData().toFloat());
    }

    /***
     * Checks if this float is less than or equal to the {@code rightFloat} free attribute
     * @param  rightFloat a number to compare this float to
     * @return A boolean representing the truth value of the comparison of this float with the {@code rightFloat} free attribute
     */
    public EObool EOleq(EOObject rightFloat) {
        return new EObool(this.value <= rightFloat._getData().toFloat());
    }

    /***
     * Checks if this float is greater than the {@code rightFloat} free attribute
     * @param  rightFloat a number to compare this float to
     * @return A boolean representing the truth value of the comparison of this float with the {@code rightFloat} free attribute
     */
    public EObool EOgreater(EOObject rightFloat) {
        return new EObool(this.value > rightFloat._getData().toFloat());
    }

    /***
     * Checks if this float is greater than or equal to the {@code rightFloat} free attribute
     * @param  rightFloat a number to compare this float to
     * @return A boolean representing the truth value of the comparison of this float with the {@code rightFloat} free attribute
     */
    public EObool EOgeq(EOObject rightFloat) {
        return new EObool(this.value >= rightFloat._getData().toFloat());
    }

    /***
     * Negates this float
     * @return A negative value of this float
     */
    public EOfloat EOneg() {
        return new EOfloat(-value);
    }

    /***
     * Makes this float a non-negative value
     * @return An object representing the absolute value of this float
     */
    public EOfloat EOabs() {
        return new EOfloat(Math.abs(this.value));
    }

    /***
     * Makes a Sign number of this float, thus, either -1.0, 0.0, or 1.0
     * @return An object representing the sign number value of this float
     */
    public EOfloat EOsignum() {
        return new EOfloat(Math.signum(this.value));
    }

    /***
     * Multiplies this float by the number of times specified by the {@code exponent} free attribute
     * @param exponent a number by which this float is to be multiplied the number of times
     * @return An object representing the  {@code exponent} free attribute)th power of this float
     */
    public EOfloat EOpow(EOObject exponent) {
        return new EOfloat(Math.pow(this.value, exponent._getData().toFloat()));
    }

}
