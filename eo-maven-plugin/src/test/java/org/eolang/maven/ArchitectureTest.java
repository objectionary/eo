/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.tngtech.archunit.core.importer.ClassFileImporter;
import com.tngtech.archunit.lang.syntax.ArchRuleDefinition;
import com.tngtech.archunit.lang.syntax.elements.GivenClassesConjunction;
import org.apache.maven.plugins.annotations.Mojo;
import org.junit.jupiter.api.Test;

/**
 * Test case for architectural conventions.
 *
 * @since 0.51.0
 */
@SuppressWarnings({"JTCOP.RuleAllTestsHaveProductionClass", "JTCOP.RuleAssertionMessage"})
final class ArchitectureTest {

    @Test
    void mojosAreInPlace() {
        ArchitectureTest.mojos()
            .should().resideInAPackage("org.eolang.maven")
            .andShould().bePublic()
            .andShould().beTopLevelClasses()
            .check(new ClassFileImporter().importPackages("org.eolang.maven"));
    }

    @Test
    void mojosHaveOneParent() {
        ArchitectureTest.mojos()
            .should()
            .beAssignableTo(MjSafe.class)
            .check(new ClassFileImporter().importPackages("org.eolang.maven"));
    }

    @Test
    void mojosHaveAnnotation() {
        ArchitectureTest.mojos()
            .should()
            .beAnnotatedWith(Mojo.class)
            .check(new ClassFileImporter().importPackages("org.eolang.maven"));
    }

    /**
     * All the project Mojos.
     * @return Mojos classes conjunction.
     */
    private static GivenClassesConjunction mojos() {
        return ArchRuleDefinition.classes()
            .that().haveSimpleNameStartingWith("Mj")
            .and().doNotHaveSimpleName("MjSafe")
            .and().haveSimpleNameNotEndingWith("Test")
            .and().haveSimpleNameNotEndingWith("IT");
    }
}
