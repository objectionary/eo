package org.eolang.maven.optimization;

import com.jcabi.xml.XML;
import java.nio.file.Path;
import org.cactoos.Func;

public final class OptLambda implements Optimization {

    private final Func<Path, XML> delegate;

    public OptLambda(final Func<Path, XML> delegate) {
        this.delegate = delegate;
    }

    @Override
    public XML optimize(final Path xml) throws OptimizationException {
        try {
            return delegate.apply(xml);
        } catch (Exception ex) {
            throw new OptimizationException(
                String.format("Can't apply optimization for '%s'", xml),
                ex
            );
        }
    }
}
