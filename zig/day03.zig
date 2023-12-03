const std = @import("std");

const Allocator = std.mem.Allocator;

pub fn main() !void {
    std.debug.print("total: {d}\n", .{solve(@embedFile("input03.txt"))});
}

fn solve(input: []const u8) u32 {
    var lines = std.mem.split(u8, input, "\n");
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var grid = std.ArrayList([]const u8).init(allocator);
    defer grid.deinit();

    var gears_x = std.ArrayList(u32).init(allocator);
    var gears_y = std.ArrayList(u32).init(allocator);

    {
        var y: u32 = 0;
        while (lines.next()) |line| {
            grid.append(line) catch return 0;
            var x: u32 = 0;
            for (line) |c| {
                if (c == '*') {
                    gears_x.append(x) catch return 0;
                    gears_y.append(y) catch return 0;
                }
                x += 1;
            }
            y += 1;
        }
    }

    var total: u32 = 0;
    var i: u32 = 0;
    while (i < gears_x.items.len) : (i += 1) {
        var x = gears_x.items[i];
        var y = gears_y.items[i];
        var dy = y - 1;
        var gear_ratio: u32 = 1;
        var gears: u32 = 0;
        while (dy <= y + 1) : (dy += 1) {
            var dx = x - 1;
            while (dx <= x + 1) : (dx += 1) {
                if (std.ascii.isDigit(grid.items[dy][dx])) {
                    if (dx > x - 1 and std.ascii.isDigit(grid.items[dy][dx - 1])) {
                        continue;
                    }
                    var n = number_at(grid, dy, dx);
                    gear_ratio *= n;
                    gears += 1;
                    std.debug.print("{d}", .{n});
                } else {
                    std.debug.print("{c}", .{grid.items[dy][dx]});
                }
            }
            std.debug.print("\n", .{});
        }
        if (gears >= 2) {
            total += gear_ratio;
        }
        std.debug.print("\n\n\n", .{});
    }

    return total;
}

fn number_at(grid: std.ArrayList([]const u8), dy: u32, dx: u32) u32 {
    var x = dx;
    while (x > 0 and std.ascii.isDigit(grid.items[dy][x - 1])) {
        // std.debug.print("[{s}\n", .{grid.items[dy][x..]});
        x -= 1;
    }
    var x_max = x;
    while (x_max < grid.items[dy].len - 1 and std.ascii.isDigit(grid.items[dy][x_max + 1])) {
        // std.debug.print("]{s}\n", .{grid.items[dy][x..x_max + 1]});
        x_max += 1;
    }
    
    // std.debug.print("<{s}> [{d}][{d}]\n", .{grid.items[dy][x..x_max + 1], x, x_max});
    return std.fmt.parseInt(u32, grid.items[dy][x..x_max + 1], 10) catch 0;
}
