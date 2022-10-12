/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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
 */
package EOorg.EOeolang;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import org.eolang.AtComposite;
import org.eolang.AtFree;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * JAVA.
 *
 * @todo #217:30min Currenlty this can only access
 *  static fields. Add ability to call static methods
 *  also. After that start to implement the same
 *  functionality for instance methods.
 * @since 1.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "java")
public class EOjava extends PhDefault {

    /**
     * Ctor.
     * @param sigma Sigma
     */
    public EOjava(final Phi sigma) {
        super(sigma);
        this.add("class", new AtFree());
        this.add(
            "φ",
            new AtComposite(
                this,
                rho -> new PhJavaStaticFields(
                    Class.forName(
                        new Dataized(
                            rho
                                .attr("class")
                                .get()
                        ).take(String.class)
                    ),
                    rho
                )
            )
        );
    }

    /**
     * Extract static fields to attributes.
     * @since 1.0
     */
    public static class PhJavaStaticFields extends PhDefault {

        /**
         * Ctor.
         * @param type Object type
         * @param parent Parent
         */
        PhJavaStaticFields(final Class<?> type, final Phi parent) {
            super(parent);
            for (final Field field :type.getDeclaredFields()) {
                if (Modifier.isStatic(field.getModifiers())) {
                    this.add(
                        field.getName(),
                        new AtComposite(
                            this,
                            self -> new Data.ToPhi(field.get(null))
                        )
                    );
                }
            }
        }
    }

}
