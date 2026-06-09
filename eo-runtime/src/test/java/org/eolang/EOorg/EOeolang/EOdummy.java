/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang.EOorg.EOeolang; // NOPMD

import org.eolang.PhDefault;
import org.eolang.XmirObject;

/**
 * Fixture object sitting deep in the {@code org.eolang.EOorg.EOeolang} Java
 * package, used to verify that {@link PhDefault#forma()} drops only the single
 * runtime root {@code org.eolang} prefix and keeps the rest of the EO package.
 * @since 0.73.1
 */
@XmirObject(oname = "dummy")
public final class EOdummy extends PhDefault {
}
