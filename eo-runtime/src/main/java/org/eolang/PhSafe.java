/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.Objects;
import java.util.function.Supplier;

/**
 * An object with coordinates (line and position) and a safe
 * processing of any runtime errors.
 *
 * <p>It is used to wrap any object and provide a safe processing
 * of any runtime errors. It is used in the EO runtime to provide
 * a safe processing of any runtime errors in the EO code. If, in any
 * method invocation, a runtime error occurs, it is caught and wrapped
 * into an {@link ExFailure} carrying the location of the error in the
 * EO code. Only {@link EOrecovered} intercepts an {@link ExFailure}, so
 * the failure keeps propagating until it either reaches a recovery or
 * terminates the program.</p>
 *
 * <p>An {@link ExInterrupted} and a JVM {@link Error} are the exceptions to
 * this: they pass through untouched, keeping their own type. Neither is an
 * EO-level termination the program may recover from — an
 * {@link ExInterrupted} is a signal that this thread must stop, and an
 * {@link Error} such as {@link StackOverflowError} or
 * {@link OutOfMemoryError} means the JVM itself can no longer be trusted to
 * carry on — and wrapping either into an {@link ExFailure} would let the
 * nearest {@link EOrecovered} intercept it. An {@link ExAgain} passes
 * through for the opposite reason: it is not a failure at all, but the
 * signal by which a {@link PhAgain} hands the next iteration to the
 * {@link PhLoop} around its formation.</p>
 *
 * <p>Elsewhere we let Cactoos catch for us, with {@code ScalarWithFallback}.
 * Here we catch by hand, because {@code eo-runtime} ships with no
 * compile-scope dependencies at all.</p>
 *
 * @since 0.21
 */
public final class PhSafe implements Phi, Atom {

    /**
     * The original.
     */
    private final Phi origin;

    /**
     * EO program name (the name of the {@code .eo} file).
     */
    private final String program;

    /**
     * The line number.
     */
    private final int line;

    /**
     * The position in the line.
     */
    private final int position;

    /**
     * The location.
     */
    private final String location;

    /**
     * The original name.
     */
    private final String oname;

    /**
     * Ctor.
     * @param phi The object
     */
    public PhSafe(final Phi phi) {
        this(phi, "unknown", 0, 0);
    }

    /**
     * Ctor.
     * @param phi The object
     * @param prg Name of the program
     * @param lne Line
     * @param pos Position
     */
    public PhSafe(final Phi phi, final String prg, final int lne, final int pos) {
        this(phi, prg, lne, pos, "?", "?");
    }

    /**
     * Ctor.
     * @param phi The object
     * @param prg Name of the program
     * @param lne Line
     * @param pos Position
     * @param loc Location
     * @param oname Original name
     */
    public PhSafe(
        final Phi phi, final String prg, final int lne,
        final int pos, final String loc, final String oname
    ) {
        this.origin = phi;
        this.program = prg;
        this.line = lne;
        this.position = pos;
        this.location = loc;
        this.oname = oname;
    }

    @Override
    public Phi copy() {
        return new PhSafe(
            this.origin.copy(), this.program,
            this.line, this.position, this.location, this.oname
        );
    }

    @Override
    public boolean needsRho() {
        return this.through(this.origin::needsRho);
    }

    @Override
    public Phi take(final String name) {
        return this.through(() -> this.origin.take(name));
    }

    @Override
    public void put(final int pos, final Phi object) {
        this.act(() -> this.origin.put(pos, object));
    }

    @Override
    public void put(final String nme, final Phi object) {
        this.act(() -> this.origin.put(nme, object));
    }

    @Override
    public String locator() {
        return String.format("%s:%d:%d", this.location, this.line, this.position);
    }

    @Override
    public String forma() {
        return this.origin.forma();
    }

    @Override
    public byte[] delta() {
        return this.through(this.origin::delta, ".Δ");
    }

    @Override
    public Phi normalized() {
        final Phi normal = this.through(this.origin::normalized);
        final Phi result;
        if (normal instanceof PhTerminator) {
            result = normal;
        } else {
            result = new PhSafe(
                normal, this.program, this.line, this.position, this.location, this.oname
            );
        }
        return result;
    }

    @Override
    public Phi lambda() {
        return this.through(new AtomSafe(this.origin)::lambda, ".λ");
    }

    @Override
    public String φTerm() {
        return this.through(this.origin::φTerm);
    }

    private void act(final Runnable action) {
        this.through(
            () -> {
                action.run();
                return true;
            },
            ""
        );
    }

    private <T> T through(final Supplier<T> action) {
        return this.through(action, "");
    }

    // @checkstyle IllegalCatchCheck (20 lines)
    @SuppressWarnings({"PMD.AvoidCatchingGenericException", "java:S1181"})
    private <T> T through(final Supplier<T> action, final String suffix) {
        try {
            return action.get();
        } catch (final ExInterrupted | ExAgain | Error ex) {
            throw ex;
        } catch (final Throwable ex) {
            throw new ExFailure(
                String.format("%s; %s", this.label(suffix), PhSafe.message(ex)),
                ex
            );
        }
    }

    private static String message(final Throwable exp) {
        return Objects.toString(exp.getMessage(), exp.getClass().getName());
    }

    private String label(final String suffix) {
        return String.format(
            "Error in \"%s%s\" at %s:%d:%d",
            this.location, suffix, this.program, this.line, this.position
        );
    }
}
