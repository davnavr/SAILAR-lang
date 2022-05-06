namespace SAILARSharp.Tests {
    using NUnit.Framework;
    using SAILARSharp.Reader;

    public class ReaderTests {
        [Test]
        public void Test1() {
            var contents = new byte[] { (byte)'S', (byte)'A', (byte)'I', (byte)'L', (byte)'A', (byte)'R', 0, 12, 0, 0 };

            using var reader = new ModuleReader(contents);
            Assert.Equals(reader.GetModuleFormat().GetMajorFormatVersion(), 0);
            Assert.Equals(reader.GetModuleFormat().GetMinorFormatVersion(), 12);
            Assert.Equals(reader.GetModuleFormat().GetIntegerByteSize(), 1);
        }
    }
}
