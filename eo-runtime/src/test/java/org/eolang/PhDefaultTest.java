/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import com.yegor256.Together;
import java.util.concurrent.atomic.AtomicInteger;
import org.cactoos.set.SetOf;
import org.eolang.EO_org.EO_eolang.EOdummy;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link PhDefault}.
 * @since 0.1
 */
final class PhDefaultTest {

    @Test
    void printsEmptyObjectAsTerm() {
        MatcherAssert.assertThat(
            "Empty object must render as empty brackets in φ-term, but it didnt",
            new PhDefault().φTerm(),
            Matchers.equalTo("[]")
        );
    }

    @Test
    void printsDataAsTerm() {
        MatcherAssert.assertThat(
            "Object with data must render its bytes in φ-term, but it didnt",
            new PhDefault(new byte[] {(byte) 0x01, (byte) 0x02}).φTerm(),
            Matchers.equalTo("[D> 01-02]")
        );
    }

    @Test
    void printsSingleByteDataAsTerm() {
        MatcherAssert.assertThat(
            "Object with one data byte must render it with a trailing dash in φ-term, matching the R-3.13.1 BYTES literal form",
            new PhDefault(new byte[] {(byte) 0x01}).φTerm(),
            Matchers.equalTo("[D> 01-]")
        );
    }

    @Test
    void printsVoidAttributeAsTerm() {
        MatcherAssert.assertThat(
            "Object with unset void attribute must render it as question mark, but it didnt",
            new PhDefault(new Attrs(new Attr("x", new AtVoid("x")))).φTerm(),
            Matchers.equalTo("[x->?]")
        );
    }

    @Test
    void printsUnsetNumberStructurally() {
        MatcherAssert.assertThat(
            "Number without injected bytes must fall back to its structural φ-term, but it didnt",
            Phi.Φ.take("number").φTerm(),
            Matchers.containsString("as-bytes->?")
        );
    }

    @Test
    void comparesTwoObjects() {
        final Phi phi = PhInt.made();
        MatcherAssert.assertThat(
            "Object should be equal to itself",
            phi, Matchers.equalTo(phi)
        );
    }

    @Test
    void comparesSelfToCopy() {
        final Phi phi = PhInt.made();
        MatcherAssert.assertThat(
            "Object should not be equal to its copy",
            phi, Matchers.not(Matchers.equalTo(phi.copy()))
        );
    }

    @Test
    void comparesTwoCopies() {
        final Phi phi = PhInt.made();
        MatcherAssert.assertThat(
            "Two copies of object should be equal to each other",
            phi.copy(), Matchers.not(Matchers.equalTo(phi.copy()))
        );
    }

    @Test
    void doesNotHaveRhoWhenFormed() {
        MatcherAssert.assertThat(
            String.format("Object should not have %s attribute when it's just formed", Phi.RHO),
            PhInt.made().take(Phi.RHO),
            Matchers.instanceOf(PhTerminator.class)
        );
    }

    @Test
    void setsRhoAfterDispatch() {
        final Phi phi = PhInt.made();
        MatcherAssert.assertThat(
            String.format("Kid should have %s attribute after dispatch", Phi.RHO),
            phi.take(this.plus()).take(Phi.RHO),
            Matchers.equalTo(phi)
        );
    }

    @Test
    void doesNotHaveRhoAfterCopying() {
        MatcherAssert.assertThat(
            String.format("Object should not give %s attribute after copying", Phi.RHO),
            PhInt.made().copy().take(Phi.RHO),
            Matchers.instanceOf(PhTerminator.class)
        );
    }

    @Test
    void needsRhoWhenItIsDeclaredAsVoid() {
        MatcherAssert.assertThat(
            String.format("Object with %s declared as void must await a receiver", Phi.RHO),
            new PhDefault(new Attrs(new Attr(Phi.RHO, new AtVoid(Phi.RHO)))).needsRho(),
            Matchers.is(true)
        );
    }

    @Test
    void bindsHostToRhoDeclaredAsVoid() {
        final Phi host = PhInt.made();
        host.put(Phi.RHO, Phi.Φ);
        host.put(this.getVoid(), PhInt.formation());
        MatcherAssert.assertThat(
            String.format("Dispatch must bind the host to %s declared as void", Phi.RHO),
            host.take(this.getVoid()).take(Phi.RHO),
            Matchers.equalTo(host)
        );
    }

    @Test
    void copiesKid() {
        final Phi phi = PhInt.made();
        MatcherAssert.assertThat(
            "Child attributes should be copied after copying main object",
            phi.take(this.plus()),
            Matchers.not(
                Matchers.equalTo(phi.copy().take(this.plus()))
            )
        );
    }

    @Test
    void takesDifferentAbstractKidsEveryDispatch() {
        final Phi phi = PhInt.made();
        MatcherAssert.assertThat(
            "Child attributes should be copied on every dispatch",
            phi.take(this.plus()),
            Matchers.not(
                Matchers.equalTo(phi.take(this.plus()))
            )
        );
    }

    @Test
    void hasKidWithSetRhoAfterCopying() {
        final Phi phi = PhInt.made().copy();
        final Phi plus = phi.take(this.plus());
        plus.take(Phi.RHO);
        MatcherAssert.assertThat(
            String.format(
                "%s attribute of copied child object should be equal to copied main object",
                Phi.RHO
            ),
            plus.take(Phi.RHO),
            Matchers.equalTo(phi)
        );
    }

    @Test
    void hasDifferentKidsAfterDoubleCopying() {
        final Phi phi = PhInt.made();
        final Phi first = phi.copy();
        MatcherAssert.assertThat(
            "Child objects after double copying should be different",
            first.copy().take(this.plus()),
            Matchers.not(
                Matchers.equalTo(first.take(this.plus()))
            )
        );
    }

    @Test
    void changesKidRhoAfterSelfCopyingKidShouldReferToOriginal() {
        final Phi phi = PhInt.made();
        MatcherAssert.assertThat(
            String.format(
                "%s attribute of original object kid should refer to original object", Phi.RHO
            ),
            phi.take(this.plus()).take(Phi.RHO),
            Matchers.not(Matchers.equalTo(phi.copy().take(this.plus()).take(Phi.RHO)))
        );
    }

    @Test
    void changesKidRhoAfterSelfCopyingKidShouldReferToCopied() {
        final Phi phi = PhInt.made();
        final Phi copy = phi.copy();
        phi.take(this.plus()).take(Phi.RHO);
        MatcherAssert.assertThat(
            String.format(
                "%s attribute of copied object kid should refer to copied object",
                Phi.RHO
            ),
            copy.take(this.plus()).take(Phi.RHO),
            Matchers.equalTo(copy)
        );
    }

    @Test
    void doesNotChangeRhoAfterDirectKidCopying() {
        final Phi phi = PhInt.made();
        final Phi first = phi.take(this.plus());
        MatcherAssert.assertThat(
            String.format(
                "%s attribute of kid attribute should not be changed after direct copying",
                Phi.RHO
            ),
            first.copy().take(Phi.RHO),
            Matchers.equalTo(
                first.take(Phi.RHO)
            )
        );
    }

    @Test
    void doesNotCopyRhoWhileDispatch() {
        final Phi phi = PhInt.made();
        final Phi plus = phi.take(this.plus());
        MatcherAssert.assertThat(
            String.format("%s attributes should not be copied while dispatch", Phi.RHO),
            plus.take(Phi.RHO),
            Matchers.equalTo(plus.take(Phi.RHO))
        );
    }

    @Test
    void copiesUnsetVoidAttribute() {
        Assertions.assertThrows(
            ExAbstract.class,
            () -> new Dataized(
                new PhSafe(PhInt.made()).copy().take(this.getVoid())
            ).take(),
            "Copying must preserve the unset void, which reads as a terminator and fails when dataized"
        );
    }

    @Test
    void copiesSetVoidAttributeOnCopy() {
        final Phi phi = PhInt.made();
        phi.put(this.getVoid(), new Data.ToPhi(10L));
        MatcherAssert.assertThat(
            "Copied set void attribute should be different from original one",
            phi.take(this.getVoid()),
            Matchers.not(
                Matchers.equalTo(phi.copy().take(this.getVoid()))
            )
        );
    }

    @Test
    void doesNotCopySetVoidAttributeWithRho() {
        final Phi phi = PhInt.made();
        phi.put(this.getVoid(), new Data.ToPhi(10L));
        MatcherAssert.assertThat(
            "Void attribute should not be copied with rho, but it did",
            phi.take(this.getVoid()),
            Matchers.equalTo(phi.take(this.getVoid()))
        );
    }

    @Disabled
    @Test
    void doesNotCopyContextAttributeWithRho() {
        final Phi phi = this.phiWithContextAttribute("context-doesNotCopyContextAttributeWithRho");
        MatcherAssert.assertThat(
            "Context attribute should not be copied with rho, but it did",
            phi.take("context-doesNotCopyContextAttributeWithRho"),
            Matchers.equalTo(phi.take("context-doesNotCopyContextAttributeWithRho"))
        );
    }

    @Test
    void hasAccessToDependentOnContextAttributeThrows() {
        Assertions.assertThrows(
            ExAbstract.class,
            () -> new Dataized(
                new PhSafe(PhInt.made().copy()).take(Phi.PHI)
            ).take(),
            "Phi depending on an unset void (now a terminator) must fail when dataized, but it did not"
        );
    }

    @Test
    void hasAccessToDependentOnContextAttributeDoesNotThrow() {
        final Phi phi = new PhSafe(PhInt.made().copy());
        phi.put(this.getVoid(), new Data.ToPhi(10L));
        Assertions.assertDoesNotThrow(
            () -> phi.take(Phi.PHI),
            "Phi should be accessible after setting void attribute, but it didn't"
        );
    }

    @Test
    void hasContextedChildWithSetRhoWhenFormed() {
        Assertions.assertDoesNotThrow(
            () -> PhInt.made()
                .take("context-hasContextedChildWithSetRhoWhenFormed")
                .take(Phi.RHO),
            String.format(
                "Contexted attribute should already have %s attribute",
                Phi.RHO
            )
        );
    }

    @Test
    void makesObjectIdentity() {
        MatcherAssert.assertThat(
            "Object should have a hashCode greater then 0, but it didn't",
            PhInt.made().hashCode(),
            Matchers.greaterThan(0)
        );
    }

    @Test
    void createsDifferentPhiInParallel() {
        final int threads = 100;
        MatcherAssert.assertThat(
            "all objects are unique",
            new SetOf<>(
                new Together<>(
                    threads,
                    t -> PhInt.made()
                )
            ),
            Matchers.iterableWithSize(threads)
        );
    }

    @Test
    void copiesWithSetData() {
        final String data = "Hello";
        final Phi phi = PhInt.made();
        phi.put(0, new Data.ToPhi(data));
        MatcherAssert.assertThat(
            "Copied Phi should contain the same data, but it didn't",
            new Dataized(phi.copy()).asString(),
            Matchers.equalTo(data)
        );
    }

    @Test
    void setsVoidAttributeOnlyOnce() {
        final Phi num = new Data.ToPhi(42L);
        final Phi phi = new PhParent();
        phi.put(0, num);
        Assertions.assertThrows(
            ExReadOnly.class,
            () -> phi.put(0, num),
            "Setting void attribute more than once should fail, but it didn't"
        );
    }

    @Test
    void printsEndlessRecursionObject() {
        MatcherAssert.assertThat(
            "Dataization should discover the infinite recursion, but it didn't",
            new Dataized(new PhEndless(new AtomicInteger(2))).asNumber(),
            Matchers.equalTo(0.0)
        );
    }

    @Test
    void hesPhiRecursively() {
        MatcherAssert.assertThat(
            "Dataization should discover the infinite recursion, but it didn't",
            new Dataized(PhRecursive.made(new AtomicInteger(3))).asNumber(),
            Matchers.equalTo(0.0)
        );
    }

    @Test
    void cachesPhiViaNewRecursively() {
        MatcherAssert.assertThat(
            "Does not cache phi via new recursively",
            new Dataized(new PhRecursiveNew(new AtomicInteger(3))).asNumber(),
            Matchers.equalTo(0.0)
        );
    }

    @Test
    void doesNotReadMultipleTimes() {
        final Phi phi = PhCounter.made();
        final long total = 2L;
        for (long idx = 0L; idx < total; ++idx) {
            new Dataized(phi).take();
        }
        MatcherAssert.assertThat(
            "Phi should not be read multiple times, but it was",
            new Dataized(new PhDispatch(phi, "count")).asNumber(),
            Matchers.equalTo(1.0)
        );
    }

    @Test
    void hasTheSameFormaWithBoundedData() {
        MatcherAssert.assertThat(
            "Bounded data objects should have the same forma, but they didn't",
            new Data.ToPhi(5L).forma(),
            Matchers.equalTo(new Data.ToPhi(6).forma())
        );
    }

    @Test
    void rendersFormaOnAnonymousAbstract() {
        MatcherAssert.assertThat(
            "Anonymous abstract object should be rendered without scopes",
            new PhDefault().forma(),
            Matchers.equalTo("[]")
        );
    }

    @Test
    void rendersFormaFromXmirLocator() {
        MatcherAssert.assertThat(
            "Named nested object must report the forma taken from its XMIR locator, but it didnt",
            new PhDefault("Φ.chunk").forma(),
            Matchers.equalTo("Φ.chunk")
        );
    }

    @Test
    void dropsRuntimeRootPackageFromForma() {
        MatcherAssert.assertThat(
            "forma of a root atom must drop the org.eolang runtime package, but it didnt",
            new Data.ToPhi(42L).forma(),
            Matchers.equalTo("Φ.number")
        );
    }

    @Test
    void keepsSubPackageInForma() {
        MatcherAssert.assertThat(
            "forma must keep the EO sub-package without its EO marker, but it didnt",
            new EOstring$EOregex$EOcompile().forma(),
            Matchers.equalTo("Φ.string.regex.compile")
        );
    }

    @Test
    void keepsSingleRootPrefixForDeepPackage() {
        MatcherAssert.assertThat(
            "forma of org.eolang.EO_org.EO_eolang object must keep a single org.eolang prefix, but it didnt",
            new EOdummy().forma(),
            Matchers.equalTo("Φ.org.eolang.dummy")
        );
    }

    @Test
    void hasDifferentFormaWithBoundedMethod() {
        final Phi five = new Data.ToPhi(5L);
        MatcherAssert.assertThat(
            "Phi and bounded method result should have different formas, but they were the same",
            five.forma(),
            Matchers.not(
                Matchers.equalTo(
                    new PhApplication(
                        five.take(this.plus()).copy(),
                        "x",
                        new Data.ToPhi(5)
                    ).forma()
                )
            )
        );
    }

    @Test
    void hasTheSameFormaWithDifferentInstances() {
        MatcherAssert.assertThat(
            "Similar Phis with different data should have the same forma, but they didn't",
            new PhApplication(
                new Data.ToPhi(5L).take(this.plus()).copy(),
                "x",
                new Data.ToPhi(5L)
            ).forma(),
            Matchers.equalTo(
                new PhApplication(
                    new Data.ToPhi(6L).take(this.plus()).copy(),
                    "x",
                    new Data.ToPhi(6L)
                ).forma()
            )
        );
    }

    @Test
    void injectsPhi() {
        final Phi phi = new PhVoidPhi();
        phi.put(0, new Data.ToPhi(5));
        MatcherAssert.assertThat(
            "Object must be injected to phi attribute and dataized",
            new Dataized(phi).asNumber().intValue(),
            Matchers.equalTo(5)
        );
    }

    @Test
    void doesNotCalculateRandomTwice() {
        final Phi rnd = new PhApplication(
            new PhDispatch(
                new PhApplication(
                    new PhDispatch(
                        new PhRnd(), this.plus()
                    ),
                    0, new Data.ToPhi(1.2)
                ),
                this.plus()
            ),
            0, new Data.ToPhi(1.2)
        );
        MatcherAssert.assertThat(
            "Random value should be the same on second access, but it wasn't",
            new Dataized(rnd).asNumber(),
            Matchers.equalTo(new Dataized(rnd).asNumber())
        );
    }

    @Test
    void failsCorrectlyWhenTooManyAttributesPut() {
        MatcherAssert.assertThat(
            "the message explains what's going on",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new PhCached().put(1, new Data.ToPhi(1)),
                "fails when trying to set attribute with too big position"
            ).getMessage(),
            Matchers.containsString("Can't overwrite the cached attribute ")
        );
    }

    @Test
    void verifiesThreadLocalNesting() {
        Assertions.assertDoesNotThrow(
            () -> this.phiWithContextAttribute("context-verifiesThreadLocalNesting")
                .take("context-verifiesThreadLocalNesting"),
            "Nesting should be properly managed without exceptions"
        );
    }

    @Test
    void returnsTerminatorForAbsentAttribute() {
        MatcherAssert.assertThat(
            "Taking an absent attribute must return a terminator, not throw",
            this.phiWithContextAttribute(
                "context-returnsTerminatorForAbsentAttribute"
            ).take("non-existent-attribute"),
            Matchers.instanceOf(PhTerminator.class)
        );
    }

    @Test
    void verifiesThreadLocalNestingWithExceptionsDoesNotThrow() {
        Assertions.assertDoesNotThrow(
            () -> this.phiWithContextAttribute(
                "context-verifiesThreadLocalNestingWithExceptions"
            ).take("context-verifiesThreadLocalNestingWithExceptions"),
            "Should still work after exception"
        );
    }

    @Test
    void worksAfterCleanup() {
        final Phi phi = this.phiWithContextAttribute("context-worksAfterCleanup");
        phi.take("context-worksAfterCleanup");
        cleansUpNesting();
        Assertions.assertDoesNotThrow(
            () -> phi.take("context-worksAfterCleanup"),
            "Should work after cleanup"
        );
    }

    @Test
    void letsSubclassObserveConstructorSuppliedAttributeOnFirstAccess() {
        final Observant kid = new Observant();
        kid.take("x");
        MatcherAssert.assertThat("constructor attribute lost", kid.seen(), Matchers.hasItems("x"));
    }

    @Test
    void letsSubclassObserveExplicitAttributeAddition() {
        final Observant kid = new Observant();
        kid.add("extra", new AtVoid("extra"));
        MatcherAssert.assertThat("added attribute lost", kid.seen(), Matchers.hasItems("extra"));
    }

    @Test
    void doesNotDuplicateOrderWhenTheSameAttributeIsAddedTwice() {
        final PhDefault dup = new PhDefault();
        dup.add("x", new AtVoid("x"));
        dup.add("x", new AtVoid("x"));
        Assertions.assertThrows(
            ExFailure.class,
            () -> dup.put(1, new Data.ToPhi(5.0)),
            "a put past the only attribute must be rejected"
        );
    }

    @Test
    void verifiesThreadLocalInMultipleThreads() {
        final int threads = 10;
        final int cnt = 100;
        final boolean[] res = new boolean[threads];
        final Thread[] pool = new Thread[threads];
        for (int idx = 0; idx < threads; idx += 1) {
            final int id = idx;
            pool[idx] = new Thread(
                () -> {
                    final Phi phi = this.phiWithContextAttribute(
                        "context-verifiesThreadLocalInMultipleThreads"
                    );
                    try {
                        for (int iter = 0; iter < cnt; iter += 1) {
                            phi.take("context-verifiesThreadLocalInMultipleThreads");
                        }
                        res[id] = true;
                    } catch (final IllegalStateException ex) {
                        res[id] = false;
                    } finally {
                        cleansUpNesting();
                    }
                }
            );
            pool[idx].start();
        }
        this.joinsThreads(pool);
        for (int idx = 0; idx < threads; idx += 1) {
            Assertions.assertTrue(
                res[idx],
                String.format("Thread %d should have completed successfully", idx)
            );
        }
    }

    private void joinsThreads(final Thread... threads) {
        for (final Thread thread : threads) {
            try {
                thread.join();
            } catch (final InterruptedException ex) {
                Thread.currentThread().interrupt();
                throw new IllegalStateException("Thread interrupted", ex);
            }
        }
    }

    private Phi phiWithContextAttribute(final String attribute) {
        final PhDefault phi = new PhDefault();
        phi.add(attribute, new AtComposite(phi, rho -> new Data.ToPhi(42)));
        return phi;
    }

    private String plus() {
        return "plus";
    }

    private String getVoid() {
        return "void";
    }

    @SuppressWarnings("PMD.AvoidAccessibilityAlteration")
    private static void cleansUpNesting() {
        try {
            final java.lang.reflect.Field field =
                Class.forName("org.eolang.PhDefault").getDeclaredField("NESTING");
            field.setAccessible(true);
            final ThreadLocal<?> nesting = (ThreadLocal<?>) field.get(null);
            nesting.remove();
        } catch (final ReflectiveOperationException ex) {
            throw new IllegalStateException(ex);
        }
    }
}
