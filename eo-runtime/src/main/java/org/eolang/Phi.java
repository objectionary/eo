/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
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

package org.eolang;

/**
 * A simple object.
 *
 * <p>We call it Phi because of the name of the φ-calculus. Actually, a better
 * name would be "Object", but it's already occupied by Java. That's why
 * we call it Phi.</p>
 *
 * <p>It is guaranteed that the hash codes of different Phi are different,
 * and equal to the vertex.</p>
 *
 * @since 0.1
 */
public interface Phi extends Data {

    /**
     * The global scope object, which owns all other objects.
     *
     * @checkstyle ConstantNameCheck (5 lines)
     */
    @SuppressWarnings("PMD.FieldNamingConventions")
    Phi Φ = new PhPackage(PhPackage.GLOBAL);

    /**
     * Make a copy, leaving it at the same parent.
     *
     * @return A copy
     */
    Phi copy();

    /**
     * Returns true if object has bound rho attribute.
     * @return True if object has rho bound attribute
     */
    boolean hasRho();

    /**
     * Take object by name of the attribute.
     * @param name The name of the attribute
     * @return The object
     */
    Phi take(String name);

    /**
     * Put object by position of the attribute.
     * @param pos The position of the attribute.
     * @param object The object to put
     */
    void put(int pos, Phi object);

    /**
     * Put object by name of the attribute.
     * @param name The name of the attribute.
     * @param object The object to put
     */
    void put(String name, Phi object);

    /**
     * Get code locator of the phi.
     * @return String containing code locator
     */
    String locator();

    /**
     * Get forma of the phi.
     * @return Forma of it as {@link String}.
     */
    String forma();
}
