namespace SAILARSharp.Interop;

using System.Runtime.InteropServices;

/// <summary>
/// <p>Encapsulates an error message produced by SAILAR.</p>
/// <p>This class is thread safe.</p>
/// </summary>
public unsafe readonly struct Error {
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

    public string Message { get; }

    private Error(string message) {
        Message = message;
    }

    internal Error FromOpaque(Opaque* error) {
        var message = GetMessage(error);
        Dispose(error);

        nuint length;
        byte* buffer = GetMessageContents(message, out length);
        string contents = System.Text.Encoding.UTF8.GetString(buffer, (int)length);
        DisposeMessage(message);

        return new(contents);
    }

    public override string ToString() => Message;
}
