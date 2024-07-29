package EOorg.EOeolang.EOsys;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import org.eolang.Dataized;
import org.eolang.Phi;

public final class DispatchedLinuxSyscall implements DispatchedSyscall {
    private final Method method;

    DispatchedLinuxSyscall(final Method method) {
        this.method = method;
    }

    DispatchedLinuxSyscall(final String name) {
        this(DispatchedLinuxSyscall.getSyscall(name));
    }

    @Override
    public int call(Phi... params) {
        try {
            return (int) method.invoke(CStdLib.CSTDLIB, this.prepareParams(params));
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

    private static Method findSyscall(String name) throws NoSuchMethodException {
        for (Method m : CStdLib.CSTDLIB.getClass().getMethods()) {
            if (m.getName().equals(name)) {
                return m;
            }
        }
        throw new NoSuchMethodException(
            String.format(
                "Can't find syscall with name %s in class %s",
                name,
                CStdLib.CSTDLIB.getClass().getName()
            )
        );
    }

    private static Method getSyscall(String name) {
        try {
            return DispatchedLinuxSyscall.findSyscall(name);
        } catch (final NoSuchMethodException e) {
            throw new IllegalArgumentException(
                String.format("Can't find syscall with name \"%s\"", name),
                e
            );
        }
    }
}
