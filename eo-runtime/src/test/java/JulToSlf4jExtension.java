import org.junit.jupiter.api.extension.BeforeAllCallback;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.slf4j.bridge.SLF4JBridgeHandler;

import java.util.logging.Level;
import java.util.logging.Logger;
import static org.junit.jupiter.api.extension.ExtensionContext.Namespace.GLOBAL;

public class JulToSlf4jExtension implements BeforeAllCallback {
    private static boolean started = false;

    /**
     * Override the beforeAll() method
     */
    @Override
    public void beforeAll(ExtensionContext context) {
        if (!started) {
            started = true;
            context.getRoot().getStore(GLOBAL).put("JulToSlf4jExtension",this);
            SLF4JBridgeHandler.install();
            Logger.getLogger("").setLevel(Level.ALL);
        }
    }
}
