/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Arrays;
import java.util.Iterator;
import org.apache.maven.plugin.AbstractMojo;

/**
 * Put the members of a package inside the object it names.
 *
 * @since 0.68.0
 */
final class PpMerge implements Iterable<Class<? extends AbstractMojo>> {

    @Override
    public Iterator<Class<? extends AbstractMojo>> iterator() {
        return Arrays.<Class<? extends AbstractMojo>>asList(
            MjParse.class,
            MjMerge.class
        ).iterator();
    }
}
