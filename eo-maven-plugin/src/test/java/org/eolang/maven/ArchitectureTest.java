/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.tngtech.archunit.base.DescribedPredicate;
import com.tngtech.archunit.core.domain.JavaClasses;
import com.tngtech.archunit.core.domain.JavaConstructorCall;
import com.tngtech.archunit.core.domain.JavaModifier;
import com.tngtech.archunit.core.importer.ClassFileImporter;
import com.tngtech.archunit.lang.syntax.ArchRuleDefinition;
import com.tngtech.archunit.lang.syntax.elements.GivenClassesConjunction;
import org.apache.maven.plugins.annotations.Mojo;
import org.junit.jupiter.api.Test;

/**
 * Test case for architectural conventions.
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
            .check(ArchitectureTest.imported());
    }

    @Test
    void mojosHaveOneParent() {
        ArchitectureTest.mojos()
            .should()
            .beAssignableTo(MjSafe.class)
            .check(ArchitectureTest.imported());
    }

    @Test
    void mojosHaveAnnotation() {
        ArchitectureTest.mojos()
            .should()
            .beAnnotatedWith(Mojo.class)
            .check(ArchitectureTest.imported());
    }

    @Test
    void buildsCacheGuardOnlyOnce() {
        ArchRuleDefinition.noClasses()
            .that().haveSimpleNameNotEndingWith("Test")
            .should().callConstructorWhere(ArchitectureTest.guardBuiltPerCall())
            .because("a per-call guard hands each thread an empty lock map (#5720)")
            .check(ArchitectureTest.imported());
    }

    /**
     * All the classes of this plugin.
     * @return Imported classes
     */
    private static JavaClasses imported() {
        return new ClassFileImporter().importPackages("org.eolang.maven");
    }

    /**
     * A {@link ConcurrentCache} built anywhere but a constructor, which is
     * the #5720 mistake: the lock map is then per call instead of per run.
     * @return The predicate over constructor calls
     */
    private static DescribedPredicate<JavaConstructorCall> guardBuiltPerCall() {
        return new DescribedPredicate<JavaConstructorCall>("cache guard is built per call") {
            @Override
            public boolean test(final JavaConstructorCall call) {
                return call.getTargetOwner().isEquivalentTo(ConcurrentCache.class)
                    && !call.getOrigin().isConstructor();
            }
        };
    }

    /**
     * All the project Mojos, the concrete goals; an abstract one is a base.
     * @return Mojos classes conjunction
     */
    private static GivenClassesConjunction mojos() {
        return ArchRuleDefinition.classes()
            .that().haveSimpleNameStartingWith("Mj")
            .and().doNotHaveModifier(JavaModifier.ABSTRACT)
            .and().haveSimpleNameNotEndingWith("Test")
            .and().haveSimpleNameNotEndingWith("IT");
    }
}
