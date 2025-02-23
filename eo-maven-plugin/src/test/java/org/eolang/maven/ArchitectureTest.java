/*
 * The MIT License (MIT)
 *
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.tngtech.archunit.core.importer.ClassFileImporter;
import com.tngtech.archunit.lang.syntax.ArchRuleDefinition;
import org.apache.maven.plugins.annotations.Mojo;
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

    @Test
    void mojosHaveAnnotation() {
        ArchRuleDefinition.classes()
            .that().haveSimpleNameEndingWith("Mojo")
            .and().doNotHaveSimpleName("SafeMojo")
            .should()
            .beAnnotatedWith(Mojo.class)
            .check(new ClassFileImporter().importPackages("org.eolang.maven"));
    }

}
