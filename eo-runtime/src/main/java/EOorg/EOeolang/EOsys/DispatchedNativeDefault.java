package EOorg.EOeolang.EOsys;

import com.sun.jna.Library;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import org.eolang.Dataized;
import org.eolang.Phi;

public class DispatchedNativeDefault implements DispatchedNativeMethod {
    private final Library lib;
    private final Method method;

    DispatchedNativeDefault(final Library lib, final Method method) {
        this.lib = lib;
        this.method = method;
    }

    DispatchedNativeDefault(final Library lib, final String name) {
        this(lib, DispatchedNativeDefault.getMethod(name, lib));
    }

    @Override
    public int call(Phi... params) {
        try {
            return (int) method.invoke(this.lib, this.prepareParams(params));
        } catch (final InvocationTargetException | IllegalAccessException e) {
            throw new IllegalStateException(
                String.format("Problem while calling syscall with name \"%s\"", method.getName()),
                e
            );
        }
    }

    private Object[] prepareParams(Phi... params) {
        final Object[] prepared = new Object[params.length];
        final Class<?>[] types = this.method.getParameterTypes();
        for (int i = 0; i < params.length; i++) {
            prepared[i] = new Dataized(params[i]).take(types[i]);
        }
        return prepared;
    }

    private static Method findMethod(final String name, final Library lib) throws NoSuchMethodException {
        for (Method m : lib.getClass().getMethods()) {
            if (m.getName().equals(name)) {
                return m;
            }
        }
        throw new NoSuchMethodException(
            String.format(
                "Can't find syscall with name %s in class %s",
                name,
                lib.getClass().getName()
            )
        );
    }

    private static Method getMethod(final String name, final Library lib) {
        try {
            return DispatchedNativeDefault.findMethod(name, lib);
        } catch (final NoSuchMethodException e) {
            throw new IllegalArgumentException(
                String.format("Can't find syscall with name \"%s\"", name),
                e
            );
        }
    }
}
