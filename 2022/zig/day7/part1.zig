const std = @import("std");

const dir_t = struct {
    const Self = @This();
    parent: ?*Self = null,
    file_children_size: usize = 0,
    dir_children: ?std.ArrayList(*dir_t) = null,
    pub fn initDirChildren(self: *Self, alloc: std.mem.Allocator) !void {
        self.dir_children = std.ArrayList(*dir_t).init(alloc);
    }
    pub fn addDirChild(self: *Self, child: *dir_t) !void {
        try self.dir_children.?.append(child);
        child.parent = self;
    }
    pub fn getSize(self: *const Self) u32 {
        const dirs = self.dir_children;
        var size: u32 = 0;
        size += self.file_children_size;
        if (dirs != null) {
            for (dirs.?.items) |dir| size += dir.getSize();
        }
        return size;
    }
};

pub fn main() !void {
    const inp = try std.fs.cwd().openFile("inp.txt", .{});
    defer inp.close();
    const file_size = (try inp.stat()).size;
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer if (gpa.deinit() == .leak) @panic("Mem leak\n");
    const buf: []u8 = try inp.reader().readAllAlloc(allocator, file_size);
    defer allocator.free(buf);
    std.debug.print("{s}\n", .{buf});

    var line_iter = std.mem.tokenizeScalar(u8, buf, '\n');
    while (line_iter.next()) |line| {
        var iter = std.mem.tokenizeScalar(u8, line, ' ');
        if (std.mem.eql(u8, iter.next().?, "$")) {}
    }
}
