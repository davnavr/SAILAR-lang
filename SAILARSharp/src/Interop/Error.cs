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

    /// <summary>Attempts to retrieve a <see cref="Span{T}"/> over the UTF-8 contents of the error message.</summary>
    /// <param name="contents">
    /// When this method returns, contains a <see cref="Span{T}"/> over the message contents, or <see cref="Span{T}.Empty"/> if
    /// the message was already disposed.
    /// </param>
    /// <returns>
    /// <see langword="true"/> if the message contents were successfully retrieved; otherwise, <see langword="false"/>.
    /// </returns>
    public bool GetBytes(out Span<byte> contents) {
        lock (sync) {
            if (message == null) {
                contents = Span<byte>.Empty;
                return false;
            } else {
                nuint length;
                byte* message = GetMessageContents(this.message, out length);
                contents = new Span<byte>(message, (int)length);
                return true;
            }
        }
    }

    /// <summary>Gets a <see cref="Span{T}"/> over the UTF-8 contents of the error message.</summary>
    /// <exception cref="ObjectDisposedException">Thrown if the message was already disposed.</exception>
    public Span<byte> GetBytes() {
        Span<byte> contents;
        if (!GetBytes(out contents)) {
            throw new ObjectDisposedException(GetType().FullName);
        }
        
        return contents;
    }

    /// <summary>Gets the error message as a <see cref="String"/>.</summary>
    public override string ToString() {
        lock (sync) {
            if (message == null) {
                return "The message was already disposed.";
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
