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
package org.eolang.maven;

import java.util.Iterator;
import org.apache.maven.model.Dependency;
import org.cactoos.list.ListOf;

/**
 * Add runtime dependency if it is absent.
 *
 * @since 0.28.11
 * @todo #1361:90min Hardcoded version of EoRuntimeDependency.
 *  See the EoRuntimeDependency constructor for more info.
 *  It's much better to determine the version of the runtime library
 *  dynamically. For example, we can fetch the latest version by http
 *  or from config files.
 */
final class DcsWithRuntime implements Dependencies {

    /**
     * The dependencies source.
     */
    private final Dependencies delegate;

    /**
     * The main constructor.
     *
     * @param delegate Dependencies source
     */
    DcsWithRuntime(final Dependencies delegate) {
        this.delegate = delegate;
    }

    @Override
    public Iterator<Dependency> iterator() {
        final ListOf<Dependency> all = new ListOf<>(this.delegate);
        if (all.stream().noneMatch(new RuntimeDependencyEquality())) {
            final Dependency dependency = new Dependency();
            dependency.setGroupId("org.eolang");
            dependency.setArtifactId("eo-runtime");
            dependency.setVersion("0.28.10");
            dependency.setClassifier("");
            all.add(dependency);
        }
        return all.iterator();
    }
}
