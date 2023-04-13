/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
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
package org.eolang.maven.tojos;

import com.yegor256.tojos.Tojo;
import com.yegor256.tojos.Tojos;
import java.io.IOException;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import org.eolang.maven.AssembleMojo;

/**
 * Tojos with additional scope attribute.
 *
 * @since 0.30
 */
public final class ScopedTojos implements Tojos {

    /**
     * Unscoped tojos without 'scope' attribute.
     */
    private final Tojos unscoped;

    /**
     * Scope.
     */
    private final String scope;

    /**
     * Ctor.
     * @param delegate Unscoped tojos without 'scope' attribute.
     * @param scope Scope.
     */
    public ScopedTojos(
        final Tojos delegate,
        final String scope
    ) {
        this.unscoped = delegate;
        this.scope = scope;
    }

    public Tojo add(final String name) {
        final Tojo tojo = this.unscoped.add(name);
        if (!tojo.exists(AssembleMojo.ATTR_SCOPE)) {
            tojo.set(AssembleMojo.ATTR_SCOPE, this.scope);
        }
        return tojo;
    }

    public List<Tojo> select(final Predicate<Tojo> filter) {
        return this.unscoped.select(
            t -> filter.test(t)
                && (t.get(AssembleMojo.ATTR_SCOPE).equals(this.scope) || "test".equals(this.scope)
            )
        );
    }

    public ForeignTojos toForeignTojos() {
        return new ForeignTojos(() -> this);
    }

    public void close() throws IOException {
        this.unscoped.close();
    }
}
