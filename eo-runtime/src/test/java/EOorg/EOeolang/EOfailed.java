/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (10 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang; // NOPMD

import com.jcabi.log.Logger;
import org.eolang.PhDefault;

/**
 * Wrong object that can't be used and instantiated.
 *
 * <p>Used in {@code org.eolang.PhPackageTest#throwsExceptionIfCantInstantiateObject()}.</p>
 *
 * @since 0.29
 */
@SuppressWarnings({
    "JTCOP.RuleAllTestsHaveProductionClass",
    "JTCOP.RuleCorrectTestName",
    "JTCOP.RuleInheritanceInTests"
})
public final class EOfailed extends PhDefault {
    /**
     * Ctor.
     * @param arg Argument
     */
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    public EOfailed(final String arg) {
        super();
        Logger.info(this, "EOfailed %s", arg);
    }
}
