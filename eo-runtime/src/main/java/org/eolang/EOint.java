package org.eolang;

import org.eolang.core.EOObject;
import org.eolang.core.data.EOData;

/***
 * Represents an integer number
 * @version %I%, %G%
 */
public class EOint extends EOObject {

    private final long value;

    public EOint(long value) {
        this.value = value;
    }

    @Override
    public EOData _getData() {
        return new EOData(value);
    }

    /***
     * Sums this integer and the {@code rightAddend} free attribute
     * @param rightAddend a number to be added to this integer
     * @return An object representing a sum of this integer and the {@code rightAddend} free attribute
     */
    public EOint EOadd(EOObject rightAddend) {
        return new EOint(this.value + rightAddend._getData().toInt());
    }

    /***
     * Subtracts the {@code subtrahend} free attribute from this integer
     * @param subtrahend a number to be subtracted from this integer
     * @return An object representing a difference of this integer and the {@code subtrahend} free attribute
     */
    public EOint EOsub(EOObject subtrahend) {
        return new EOint(this.value - subtrahend._getData().toInt());
    }

    public EOint EOdiv(EOObject divisor) {
        if(divisor._getData().toInt() == 0){
            throw new ArithmeticException("Division by zero not possible");
        }
        return new EOint(Math.floorDiv(this.value, divisor._getData().toInt()));
    }

    public EOint EOmul(EOObject rightFactor) {
        return new EOint(this.value * rightFactor._getData().toInt());
    }

    /***
     * Checks if this int is equal to the {@code rightInt} free attribute
     * @param  rightInt a number to compare this int to
     * @return A boolean representing the truth value of the comparison of this int with the {@code rightInt} free attribute
     */
    public EObool EOeq(EOObject rightInt) {
        return new EObool(this.value == rightInt._getData().toInt());
    }

    /***
     * Checks if this int is not equal to the {@code rightInt} free attribute
     * @param  rightInt a number to compare this int to
     * @return A boolean representing the truth value of the comparison of this int with the {@code rightInt} free attribute
     */
    public EObool EOneq(EOObject rightInt) {
        return new EObool(this.value != rightInt._getData().toInt());
    }

    /***
     * Checks if this int is less than the {@code rightInt} free attribute
     * @param  rightInt a number to compare this int to
     * @return A boolean representing the truth value of the comparison of this int with the {@code rightInt} free attribute
     */
    public EObool EOless(EOObject rightInt) {
        return new EObool(this.value < rightInt._getData().toInt());
    }

    /***
     * Checks if this int is less than or equal to the {@code rightInt} free attribute
     * @param  rightInt a number to compare this int to
     * @return A boolean representing the truth value of the comparison of this int with the {@code rightInt} free attribute
     */
    public EObool EOleq(EOObject rightInt) {
        return new EObool(this.value <= rightInt._getData().toInt());
    }

    /***
     * Checks if this int is greater than the {@code rightInt} free attribute
     * @param  rightInt a number to compare this int to
     * @return A boolean representing the truth value of the comparison of this int with the {@code rightInt} free attribute
     */
    public EObool EOgreater(EOObject rightInt) {
        return new EObool(this.value > rightInt._getData().toInt());
    }

    /***
     * Checks if this int is greater than or equal to the {@code rightInt} free attribute
     * @param  rightInt a number to compare this int to
     * @return A boolean representing the truth value of the comparison of this int with the {@code rightInt} free attribute
     */
    public EObool EOgeq(EOObject rightInt) {
        return new EObool(this.value >= rightInt._getData().toInt());
    }

    /***
     * Negates this int
     * @return A negative value of this int
     */
    public EOint EOneg() {
        return new EOint(-value);
    }

    /***
     * Makes this int a non-negative value
     * @return An object representing the absolute value of this int
     */
    public EOint EOabs() {
        return new EOint(Math.abs(this.value));
    }

    /***
     * Makes a Sign number of this int, thus, either -1.0, 0.0, or 1.0
     * @return An object representing the sign number value of this int
     */
    public EOint EOsignum() {
        return new EOint((long) Math.signum(this.value));
    }

    /***
     * Multiplies this int by the number of times specified by the {@code exponent} free attribute
     * @param exponent a number by which this int is to be multiplied the number of times
     * @return An object representing the  ({@code exponent} free attribute)th power of this int
     */
    public EOint EOpow(EOObject exponent) {
        return new EOint((long) Math.pow(this.value, exponent._getData().toInt()));
    }

    /***
     * Divides this int by the {@code divisor} free attribute
     * @param divisor a number by which this int is to be divided
     * @return An object representing the remainder of the division by  ({@code divisor} free attribute)
     */
    public EOint EOmod(EOObject divisor) {
        return new EOint(Math.floorMod(this.value, divisor._getData().toInt()));
    }

}
