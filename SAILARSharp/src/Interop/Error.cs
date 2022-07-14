namespace SAILARSharp.Interop;

using System.Runtime.InteropServices;

internal unsafe static class Error {
    internal readonly struct Opaque { }

    internal readonly struct OpaqueMessage { }

    [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_dispose_error", ExactSpelling = true)]
    private static extern void Dispose(Opaque* error);

    [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_dispose_error_message", ExactSpelling = true)]
    private static extern void DisposeMessage(OpaqueMessage* message);

    [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_error_message", ExactSpelling = true)]
    private static extern OpaqueMessage* GetMessage(Opaque* error);

    [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_error_message_contents", ExactSpelling = true)]
    private static extern byte* GetMessageContents(OpaqueMessage* message, out nuint length);

    internal static void ThrowOnError(Opaque* error) {
        if (error != null) {
            var message = GetMessage(error);
            Dispose(error);

            nuint length;
            byte* buffer = GetMessageContents(message, out length);
            throw new ErrorMessageException(System.Text.Encoding.UTF8.GetString(buffer, (int)length));
        }
    }
}
