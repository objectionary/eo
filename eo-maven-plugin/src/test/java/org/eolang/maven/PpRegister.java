/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Collections;
import java.util.Iterator;
import org.apache.maven.plugin.AbstractMojo;

/**
 * Single register phase.
 *
 * @since 0.1.0
 */
final class PpRegister implements Iterable<Class<? extends AbstractMojo>> {

    @Override
    public Iterator<Class<? extends AbstractMojo>> iterator() {
        return Collections.<Class<? extends AbstractMojo>>singletonList(
            MjRegister.class
        ).iterator();
    }
}
