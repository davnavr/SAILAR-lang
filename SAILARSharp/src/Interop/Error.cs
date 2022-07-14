namespace SAILARSharp.Interop;

using System.Runtime.InteropServices;

internal unsafe static class Error {
    internal readonly struct Opaque { }

    internal readonly struct OpaqueMessage { }

    [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_error_dispose", ExactSpelling = true)]
    private static extern void Dispose(Opaque* error);

    [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_error_message_dispose", ExactSpelling = true)]
    private static extern void DisposeMessage(OpaqueMessage* message);

    [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_error_message", ExactSpelling = true)]
    private static extern OpaqueMessage* GetMessage(Opaque* error);

    [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_error_message_contents", ExactSpelling = true)]
    private static extern byte* GetMessageContents(OpaqueMessage* message, out nuint length);

    /// <summary>Retrieves the error message as a <see cref="string"/>, disposing the <paramref name="error"/>.</summary>
    internal static string IntoString(Opaque* error) {
        if (error == null) {
            return string.Empty;
        }

        var message = GetMessage(error);
        nuint length;
        byte* buffer = GetMessageContents(message, out length);

        DisposeMessage(message);
        Dispose(error);

        return System.Text.Encoding.UTF8.GetString(buffer, (int)length);
    }

    /// <summary>Throws an exception if an error occured, disposing the <paramref name="error"/>.</summary>
    /// <exception cref="ErrorMessageException">Thrown if <paramref name="error"/> was not <see langword="null"/>.</exception>
    internal static void Throw(Opaque* error) {
        if (error != null) {
            throw new Core.ErrorMessageException(IntoString(error));
        }
    }
}
