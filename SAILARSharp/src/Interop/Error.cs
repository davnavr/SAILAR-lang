namespace SAILARSharp.Interop;

using System;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading;

/// <summary>
/// <p>Encapsulates an error message produced by SAILAR.</p>
/// <p>This class is thread safe.</p>
/// </summary>
public unsafe sealed class Error : IDisposable {
    internal readonly struct Opaque { }

    internal readonly struct OpaqueMessage { }

    private readonly object sync = new();

    private OpaqueMessage* message;

    private string? contents = null;

    [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_dispose_error", ExactSpelling = true)]
    private static extern void Dispose(Opaque* error);

    [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_dispose_error_message", ExactSpelling = true)]
    private static extern void DisposeMessage(OpaqueMessage* message);

    [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_error_message", ExactSpelling = true)]
    private static extern OpaqueMessage* GetMessage(Opaque* error);

    [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_error_message_contents", ExactSpelling = true)]
    private static extern byte* GetMessageContents(OpaqueMessage* message, out nuint length);

    internal Error(Opaque* error) {
        message = GetMessage(error);
        Dispose(error);
    }

    /// <summary>Gets a <see cref="Span{T}"/> over the UTF-8 contents of the error message.</summary>
    public Span<byte> GetBytes() {
        lock (sync) {
            if (message == null) {
                throw new ObjectDisposedException(null, "error message was disposed");
            }

            nuint length;
            byte* contents = GetMessageContents(message, out length);

            return new Span<byte>(contents, (int)length);
        }
    }

    public override string ToString() {
        lock (sync) {
            if (message == null) {
                return String.Empty;
            }

            contents ??= Encoding.UTF8.GetString(GetBytes());

            return contents;
        }
    }

    private void Dispose(bool disposing) {
        try {
            if (disposing) {
                Monitor.Enter(sync);
            }

            if (message != null) {
                DisposeMessage(message);
                message = null;
            }
        } finally {
            if (disposing) {
                Monitor.Exit(sync);
            }
        }
    }

    public void Dispose() => Dispose(true);

    ~Error() => Dispose(false);
}
