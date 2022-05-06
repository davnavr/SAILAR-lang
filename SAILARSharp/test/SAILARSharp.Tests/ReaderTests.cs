namespace SAILARSharp.Tests {
    using NUnit.Framework;
    using SAILARSharp.Reader;

    public class ReaderTests {
        [Test]
        public void BasicReaderTest() {
            var contents = new byte[] {
                (byte)'S', (byte)'A', (byte)'I', (byte)'L', (byte)'A', (byte)'R',
                0, 12, 0, 1, 2, 6, (byte)'H', (byte)'e', (byte)'l', (byte)'l', (byte)'o', (byte)'!',
            };

            using var reader = new ModuleReader(contents);
            var format = reader.GetModuleFormat();
            var record = reader.ReadNextRecord() as IdentifierRecord;

            Assert.AreEqual(format.GetMajorFormatVersion(), 0);
            Assert.AreEqual(format.GetMinorFormatVersion(), 12);
            Assert.AreEqual(format.GetIntegerByteSize(), 1);
            Assert.AreEqual(record?.ToString(), "Hello!");
        }
    }
}
