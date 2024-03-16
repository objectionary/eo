package org.eolang;

import java.util.concurrent.Callable;
import java.util.concurrent.atomic.AtomicReference;

public class AtFormed implements Attr {
    /**
     * Object.
     */
    private final AtomicReference<Phi> object;

    /**
     * Callback to retrieve object.
     */
    private final Callable<Phi> callback;

    /**
     * Ctor.
     * @param func Callback to retrieve object.
     */
    public AtFormed(final Callable<Phi> func) {
        this.callback = func;
        this.object = new AtomicReference<>(null);
    }

    @Override
    public Attr copy(final Phi self) {
        return new AtFormed(() -> this.get().copy());
    }

    @Override
    public Phi get() {
        try {
            if (this.object.get() == null) {
                this.object.set(this.callback.call());
            }
            return this.object.get();
        } catch (final InterruptedException ex) {
            Thread.currentThread().interrupt();
            throw new ExInterrupted();
            // @checkstyle IllegalCatchCheck (3 line)
        } catch (final RuntimeException ex) {
            throw ex;
        } catch (final Throwable ex) {
            throw new ExFailure(
                String.format(
                    "Unexpected error '%s' of type %s",
                    ex.getMessage(),
                    ex.getClass().getSimpleName()
                ),
                ex
            );
        }
    }

    @Override
    public void put(Phi phi) {
        throw new ExUnset("");
    }

    @Override
    public String φTerm() {
        return this.get().φTerm();
    }

    @Override
    public String toString() {
        return String.format("%sF", this.get().toString());
    }
}
