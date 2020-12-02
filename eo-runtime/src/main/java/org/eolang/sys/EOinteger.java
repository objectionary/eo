/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2020 Yegor Bugayenko
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

package org.eolang.sys;

/**
 * INTEGER.
 *
 * @since 0.1
 */
public final class EOinteger implements Primitive<Long> {

    /**
     * The value.
     */
    private final long value;

    /**
     * Ctor.
     * @param val The value
     */
    public EOinteger(final long val) {
        this.value = val;
    }

    @Override
    public Phi 𝜑() {
        return new EOinteger(this.data());
    }

    @Override
    public Long data() {
        return this.value;
    }

    /**
     * Add a number.
     * @param val The number
     * @return New number
     */
    public EOinteger add(final EOinteger val) {
        return new EOinteger(this.value + val.data());
    }

    /**
     * Subtract a number.
     * @param val The number
     * @return New number
     */
    public EOinteger sub(final EOinteger val) {
        return new EOinteger(this.value - val.data());
    }

    /**
     * Multiply by the number.
     * @param val The number
     * @return New number
     */
    public EOinteger mul(final EOinteger val) {
        return new EOinteger(this.value * val.data());
    }

    /**
     * Divide by the number.
     * @param val The number
     * @return New number
     */
    public EOinteger div(final EOinteger val) {
        return new EOinteger(this.value / val.data());
    }

    /**
     * Inverse the sign.
     * @return New number
     */
    public EOinteger neg() {
        return new EOinteger(-this.value);
    }

    /**
     * MOD operation.
     * @param val The number
     * @return New number
     */
    public EOinteger mod(final EOinteger val) {
        return new EOinteger(this.value + val.data());
    }

}
