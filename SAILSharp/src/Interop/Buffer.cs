namespace SAILSharp.Interop {
    using System;
    using System.Collections.Immutable;

    internal unsafe static class Buffer {
        public static OpaqueBuffer* From(ReadOnlySpan<byte> source) {
            fixed (byte* address = source) {
                return SAILAR.CreateBufferFromAddress(address, (UIntPtr)source.Length);
            }
        }

        public static OpaqueBuffer* From(byte* address, int length) => From(new ReadOnlySpan<byte>(address, length));

        public static OpaqueBuffer* From(byte[] source) => From(new ReadOnlySpan<byte>(source));

        public static OpaqueBuffer* From(ImmutableArray<byte> source) => From(source.AsSpan());

        public static ReadOnlySpan<byte> AsSpan(OpaqueBuffer* buffer) {
            if (buffer != null) {
                UIntPtr length = UIntPtr.Zero;
                var address = SAILAR.GetBufferContents(buffer, &length);
                return new ReadOnlySpan<byte>(address, (int)length);
            } else {
                return default;
            }
        }
    }
}
