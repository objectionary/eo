/**
 * The MIT License (MIT)
 *
 * Copyright (c) 2016 eolang.org
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
package org.eolang.compiler.syntax;

import com.google.common.base.Joiner;
import java.util.Collection;
import java.util.stream.Collectors;

/**
 * Method.
 *
 * @author Yegor Bugayenko (yegor256@gmail.com)
 * @version $Id$
 * @since 0.1
 */
public final class Method {

    /**
     * Method name.
     */
    private final String name;

    /**
     * Return type name.
     */
    private final String type;

    /**
     * Arguments.
     */
    private final Collection<Argument> arguments;

    /**
     * Ctor.
     * @param mtd Method name
     * @param args Arguments
     * @param rtp Return type
     */
    public Method(final String mtd, final Collection<Argument> args,
        final String rtp) {
        this.name = mtd;
        this.arguments = args;
        this.type = rtp;
    }

    /**
     * Convert it to Java.
     * @return Java code
     */
    public String java() {
        return String.format(
            "%s %s(%s);",
            this.type,
            this.name,
            Joiner.on(", ").join(
                this.arguments.stream().map(
                    Argument::java
                ).collect(Collectors.toList())
            )
        );
    }

}
