package org.eolang.maven.log;

import org.junit.jupiter.api.extension.AfterEachCallback;
import org.junit.jupiter.api.extension.BeforeEachCallback;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.api.extension.ParameterContext;
import org.junit.jupiter.api.extension.ParameterResolver;

public class LoggerExtension implements ParameterResolver, BeforeEachCallback, AfterEachCallback {

    private final Logs logs = new Logs();
    private final TestLoggerAppender appender = new TestLoggerAppender(this.logs);

    @Override
    public void beforeEach(final ExtensionContext extensionContext) {
        this.appender.init();
    }

    @Override
    public void afterEach(final ExtensionContext extensionContext) {
        this.appender.remove();
    }

    @Override
    public boolean supportsParameter(
        final ParameterContext param,
        final ExtensionContext extension
    ) {
        return param.getParameter().getType() == Logs.class;
    }

    @Override
    public Object resolveParameter(
        final ParameterContext param,
        final ExtensionContext extension
    ) {
        return this.logs;
    }
}
