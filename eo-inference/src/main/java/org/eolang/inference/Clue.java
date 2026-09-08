/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.io.IOException;
import java.nio.file.Path;

/**
 * One thing the checker can work out about the types of a program.
 *
 * <p>A clue reads the XMIR of a whole program and writes down what it
 * has understood, a table at a time. It never renames a type and never
 * merges two of them: every object keeps the type it was born with, and
 * everything learned goes into a table instead. A type is named by the
 * locator the parser gives every object, {@code Φ.app.inc.φ.ρ}, which is
 * unique across the whole program and needs no dictionary to be read.</p>
 *
 * <p>Following one clue is all a rule ever does, which is what lets the
 * checker grow: a new rule is a new clue in {@link Clues}, and nothing
 * around it has to change. The XMIR it reads is prepared beforehand —
 * every composite base split into one object per dispatch step — because
 * a rule can only notice an attribute being taken when the taking is an
 * object of its own.</p>
 *
 * @since 0.67.0
 */
@FunctionalInterface
public interface Clue {

    /**
     * Read the program and write down what is understood.
     *
     * @param xmirs The directory with the prepared XMIR of the whole
     *  program, since an object in one file is used by objects in others
     *  and no single file can be typed on its own
     * @param tables The directory for the tables, a document each
     * @throws IOException If a file cannot be read or written
     */
    void follow(Path xmirs, Path tables) throws IOException;
}
