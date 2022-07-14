namespace SAILARSharp.Interop;

using System;
using System.Runtime.InteropServices;
using System.Threading;

/// <summary>Represents a <see cref="byte"/> buffer.</summary>
public unsafe sealed class Buffer : IDisposable {
    internal readonly struct Opaque { }

    [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_buffer_dispose", ExactSpelling = true)]
    private static extern void Dispose(Opaque* buffer);

    [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_buffer_contents", ExactSpelling = true)]
    private static extern byte* GetContents(Opaque* buffer, out nuint length);

    private readonly object locker = new();

    private Opaque* buffer;

    internal Opaque* LockContents() {
        if (buffer == null) {
            throw new ObjectDisposedException(GetType().FullName);
        }

        Monitor.Enter(locker);

        return buffer;
    }

    internal void UnlockContents() {
        Monitor.Exit(locker);
    }

    /// <summary>Copies the contents of this buffer to an array.</summary>
    /// <exception cref="ObjectDisposedException">Thrown if the buffer was already disposed.</exception>
    public byte[] ToArray() {
        try {
            LockContents();
            nuint length;
            byte* contents = GetContents(buffer, out length);
            var bytes = new Span<byte>(contents, (int)length);
            return bytes.ToArray();
        } finally {
            UnlockContents();
        }
    }

    private void Dispose(bool disposing) {
        try {
            if (disposing) {
                Monitor.Enter(locker);
            }

            if (buffer != null) {
                Dispose(buffer);
                buffer = null;
            }
        } finally {
            if (disposing) {
                Monitor.Exit(locker);
            }
        }
    }

    public void Dispose() {
        Dispose(true);
        GC.SuppressFinalize(this);
    }


    ~Buffer() => Dispose(false);
}
