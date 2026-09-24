/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Collections;
import java.util.Iterator;
import org.apache.maven.plugin.AbstractMojo;

/**
 * Parse full pipeline.
 *
 * @since 0.28.12
 */
final class PpParse implements Iterable<Class<? extends AbstractMojo>> {

    @Override
    public Iterator<Class<? extends AbstractMojo>> iterator() {
        return Collections.<Class<? extends AbstractMojo>>singletonList(
            MjParse.class
        ).iterator();
    }
}
