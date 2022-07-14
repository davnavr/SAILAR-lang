namespace SAILARSharp.Interop;

using System;
using System.Runtime.InteropServices;

/// <summary>
/// <p>Encapsulates an error message produced by SAILAR.</p>
/// <p>This class is thread safe.</p>
/// </summary>
public unsafe sealed class Error {
    internal readonly struct Opaque { }

    internal readonly struct OpaqueMessage { }

    private readonly object sync = new();

    private OpaqueMessage* message;

    private string contents = String.Empty;

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
                return Span<byte>.Empty;
            }

            nuint length;
            byte* contents = GetMessageContents(message, out length);

            return new Span<byte>(contents, (int)length);
        }
    }
}
