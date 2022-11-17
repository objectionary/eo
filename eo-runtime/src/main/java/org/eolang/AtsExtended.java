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
package org.eolang;

import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

/**
 * Attrs which was extended.
 *
 * @since 0.1
 */
public final class AtsExtended implements Attrs {

    /**
     * The name of new attr.
     */
    private final String name;

    /**
     * The value of new attr.
     */
    private final Attr attr;

    /**
     * Attrs which be extended.
     */
    private final Map<String, Attr> origin;

    /**
     * The order of attrs.
     */
    private final List<? super String> order;

    /**
     * Ctor.
     *
     * @param name The name of origin
     * @param attr The value of origin
     * @param origin Attrs which be extended
     * @param order Order of attrs
     * @checkstyle ParameterNumberCheck (11 lines)
     */
    AtsExtended(
        final String name,
        final Attr attr,
        final Map<String, Attr> origin,
        final List<? super String> order
    ) {
        this.name = name;
        this.attr = attr;
        this.origin = origin;
        this.order = order;
    }

    @Override
    public Map<String, Attr> value() {
        if (Pattern.compile("^[a-z].*$").matcher(this.name).matches()) {
            this.order.add(this.name);
        }
        this.origin.put(this.name, this.attr);
        return this.origin;
    }
}
