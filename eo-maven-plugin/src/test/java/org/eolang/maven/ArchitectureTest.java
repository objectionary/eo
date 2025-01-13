/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
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

import com.tngtech.archunit.core.importer.ClassFileImporter;
import com.tngtech.archunit.lang.syntax.ArchRuleDefinition;
import org.junit.jupiter.api.Test;

/**
 * Test case for architectural conventions.
 *
 * @since 0.51.0
 */
@SuppressWarnings({ "JTCOP.RuleAllTestsHaveProductionClass", "JTCOP.RuleAssertionMessage" })
final class ArchitectureTest {

    @Test
    void mojosAreInPlace() {
        ArchRuleDefinition.classes()
            .that().haveSimpleNameEndingWith("Mojo")
            .and().doNotHaveSimpleName("SafeMojo")
            .should().resideInAPackage("org.eolang.maven")
            .andShould().bePublic()
            .andShould().beTopLevelClasses()
            .check(new ClassFileImporter().importPackages("org.eolang.maven"));
    }

    @Test
    void mojosHaveOneParent() {
        ArchRuleDefinition.classes()
            .that().haveSimpleNameEndingWith("Mojo")
            .and().doNotHaveSimpleName("SafeMojo")
            .should()
            .beAssignableTo(SafeMojo.class)
            .check(new ClassFileImporter().importPackages("org.eolang.maven"));
    }

}
