/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;

/**
 * A standard "hello world" EO program used as a shared test fixture.
 * @since 0.61.0
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
final class HelloWorld {

    /**
     * Path to the EO resource file.
     */
    private final String resource;

    HelloWorld() {
        this.resource = "org/eolang/maven/hello-world.eo";
    }

    String asString() {
        return new UncheckedText(
            new TextOf(new ResourceOf(this.resource))
        ).asString();
    }
}
