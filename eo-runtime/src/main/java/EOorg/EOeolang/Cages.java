/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
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

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang; // NOPMD

import java.util.concurrent.ConcurrentHashMap;
import org.eolang.ExFailure;
import org.eolang.Phi;
import org.eolang.Versionized;

/**
 * Cages for objects.
 * @since 0.36.0
 */
@Versionized
final class Cages {
    /**
     * Cages instance.
     */
    static final Cages INSTANCE = new Cages();

    /**
     * Encaged objects.
     */
    private final ConcurrentHashMap<Integer, Phi> objects;

    /**
     * Ctor.
     */
    private Cages() {
        this.objects = new ConcurrentHashMap<>(0);
    }

    /**
     * Encage object for the first time.
     * When object is encaged - locator will be generated.
     * @param object Object to encage
     * @return Locator to the object in cage
     */
    int init(final Phi object) {
        final int locator = object.hashCode();
        this.objects.putIfAbsent(locator, object);
        return locator;
    }

    /**
     * Encage object by locator.
     * @param locator Locator to the object in cage
     * @param object Object to encage
     */
    void encage(final int locator, final Phi object) {
        synchronized (this.objects) {
            if (!this.objects.containsKey(locator)) {
                throw new ExFailure(
                    String.format(
                        "Encaged object with locator %d was not initialized, can't reencage, can't encage",
                        locator
                    )
                );
            }
            final String current = this.objects.get(locator).forma();
            final String forma = object.forma();
            if (!current.equals(forma)) {
                throw new ExFailure(
                    String.format(
                        "Can't encage an object formed by %s because object formed by %s was encaged before",
                        forma,
                        current
                    )
                );
            }
            this.objects.put(locator, object);
        }
    }

    /**
     * Get object from cage by locator.
     * @param locator The locator of the object
     * @return Object
     */
    Phi get(final int locator) {
        synchronized (this.objects) {
            if (!this.objects.containsKey(locator)) {
                throw new ExFailure(
                    String.format(
                        "Object with locator %d is absent in cage, can't get",
                        locator
                    )
                );
            }
            return this.objects.get(locator);
        }
    }
}
