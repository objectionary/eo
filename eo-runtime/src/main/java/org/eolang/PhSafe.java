/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import EOorg.EOeolang.EOerror;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Supplier;

/**
 * An object with coordinates (line and position) and a safe
 * processing of any runtime errors.
 *
 * <p>It is used to wrap any object and provide a safe processing
 * of any runtime errors. It is used in the EO runtime to provide
 * a safe processing of any runtime errors in the EO code. If, in any
 * method invocation, a runtime error occurs, it is caught and wrapped
 * into {@link EOerror.ExError} with the location of the error in the
 * EO code.</p>
 *
 * @since 0.21
 */
@SuppressWarnings("PMD.TooManyMethods")
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
     *
     * @param phi The object
     * @checkstyle ParameterNumberCheck (5 lines)
     */
    public PhSafe(final Phi phi) {
        this(phi, "unknown", 0, 0);
    }

    /**
     * Ctor.
     *
     * @param phi The object
     * @param prg Name of the program
     * @param lne Line
     * @param pos Position
     * @checkstyle ParameterNumberCheck (5 lines)
     */
    public PhSafe(final Phi phi, final String prg, final int lne, final int pos) {
        this(phi, prg, lne, pos, "?", "?");
    }

    /**
     * Ctor.
     *
     * @param phi The object
     * @param prg Name of the program
     * @param lne Line
     * @param pos Position
     * @param loc Location
     * @param oname Original name
     * @checkstyle ParameterNumberCheck (5 lines)
     */
    public PhSafe(final Phi phi, final String prg, final int lne,
        final int pos, final String loc, final String oname) {
        this.origin = phi;
        this.program = prg;
        this.line = lne;
        this.position = pos;
        this.location = loc;
        this.oname = oname;
    }

    @Override
    public boolean equals(final Object obj) {
        return this.origin.equals(obj);
    }

    @Override
    public int hashCode() {
        return this.origin.hashCode();
    }

    @Override
    public Phi copy() {
        return new PhSafe(
            this.origin.copy(), this.program,
            this.line, this.position, this.location, this.oname
        );
    }

    @Override
    public boolean hasRho() {
        return this.through(this.origin::hasRho);
    }

    @Override
    public Phi take(final String name) {
        return this.through(() -> this.origin.take(name));
    }

    @Override
    public Phi take(final int pos) {
        return this.through(() -> this.origin.take(pos));
    }

    @Override
    public void put(final int pos, final Phi object) {
        this.through(() -> this.origin.put(pos, object));
    }

    @Override
    public void put(final String nme, final Phi object) {
        this.through(() -> this.origin.put(nme, object));
    }

    @Override
    public String locator() {
        return String.format("%s:%d:%d", this.location, this.line, this.position);
    }

    @Override
    public String forma() {
        return String.join(
            ".",
            this.getClass().getPackageName(),
            this.oname
        );
    }

    @Override
    public Phi copy(final Phi self) {
        return this.copy();
    }

    @Override
    public byte[] delta() {
        return this.through(this.origin::delta, ".Δ");
    }

    @Override
    public Phi lambda() {
        return this.through(new AtomSafe(this.origin)::lambda, ".λ");
    }

    /**
     * Helper, for other methods.
     * @param action The action
     */
    private void through(final Runnable action) {
        this.through(
            () -> {
                action.run();
                return true;
            },
            ""
        );
    }

    /**
     * Helper, for other methods.
     * @param action The action
     * @param <T> Type of result
     * @return Result
     */
    private <T> T through(final Supplier<T> action) {
        return this.through(action, "");
    }

    /**
     * Helper, for other methods.
     *
     * <p>No matter what happens inside the {@code action}, only
     * an instance of {@link EOerror.ExError} may be thrown out
     * of this method.</p>
     *
     * @param action The action
     * @param suffix The suffix to add to the label
     * @param <T> Type of result
     * @return Result
     * @checkstyle IllegalCatchCheck (20 lines)
     */
    @SuppressWarnings({"PMD.AvoidCatchingThrowable", "PMD.PreserveStackTrace"})
    private <T> T through(final Supplier<T> action, final String suffix) {
        try {
            return action.get();
        } catch (final EOerror.ExError ex) {
            throw new EOerror.ExError(ex, this.label(suffix));
        } catch (final ExAbstract ex) {
            throw new EOerror.ExError(
                new Data.ToPhi(ex.getMessage()),
                this.label(suffix)
            );
        } catch (final Throwable ex) {
            throw new EOerror.ExError(
                new Data.ToPhi(ex.getMessage()),
                PhSafe.trace(ex, this.label(suffix))
            );
        }
    }

    /**
     * Take stacktrace from exception.
     * @param exp The exception
     * @param head The head to add
     * @return The stacktrace
     */
    private static List<String> trace(final Throwable exp, final String head) {
        final StackTraceElement[] stack = exp.getStackTrace();
        final List<String> trace = new LinkedList<>();
        if (stack != null) {
            for (final StackTraceElement elm : stack) {
                trace.add(
                    String.format(
                        "%s#%s():%d",
                        PhSafe.shorter(elm.getClassName()),
                        elm.getMethodName(),
                        elm.getLineNumber()
                    )
                );
            }
        }
        trace.add(
            String.format(
                "%s (%s)",
                head, PhSafe.shorter(exp.getClass().getName())
            )
        );
        return trace;
    }

    /**
     * Make class name shorter.
     * @param full The full name of class
     * @return Shorter name
     */
    private static String shorter(final String full) {
        return full.replaceAll("(.)[^.]*\\.", "$1.");
    }

    /**
     * The label of the exception.
     * @param suffix The suffix to add to the label
     * @return Label
     */
    private String label(final String suffix) {
        return String.format(
            "Error in \"%s%s\" at %s:%d:%d",
            this.location, suffix, this.program, this.line, this.position
        );
    }
}
