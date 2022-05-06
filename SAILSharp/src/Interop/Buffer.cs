namespace SAILSharp.Interop {
    using System;

    // TODO: Make these just static methods, too easy to forget Dispose call.
    public unsafe ref struct Buffer {
        private OpaqueBuffer* buffer;

        public Buffer(OpaqueBuffer* buffer) {
            this.buffer = buffer;
        }

        public Buffer(ReadOnlySpan<byte> source) {
            fixed (byte* address = source) {
                buffer = SAILAR.CreateBufferFromAddress(address, (UIntPtr)source.Length);
            }
        }

        public Buffer(byte* address, int length) : this(new ReadOnlySpan<byte>(address, length)) {}

        public Buffer(byte[] source) : this(new ReadOnlySpan<byte>(source)) {}

        public OpaqueBuffer* Reference => buffer;

        public int Length => AsSpan().Length;

        public bool IsNull => buffer == null;

        public static Buffer Null => default;

        public ReadOnlySpan<byte> AsSpan() {
            if (!IsNull) {
                UIntPtr length = UIntPtr.Zero;
                var address = SAILAR.GetBufferContents(buffer, &length);
                return new ReadOnlySpan<byte>(address, (int)length);
            } else {
                return default;
            }
        }

        public void Dispose() {
            if (!IsNull) {
                SAILAR.DisposeBuffer(buffer);
                buffer = null;
            }
        }
    }
}
