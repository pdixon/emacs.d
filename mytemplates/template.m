//
//  `(file-name-nondirectory (buffer-file-name))`
//
//  Created by `(user-full-name)` on `(format-time-string "%Y-%02m-%d")`
//  Copyright (c) `(format-time-string "%Y")` `(my-organisation)`. All rights reserved.
//

#import "`(file-name-nondirectory (file-name-sans-extension (buffer-file-name)))`.h"

@interface ${1:`(file-name-nondirectory (file-name-sans-extension (buffer-file-name)))`}()

@end

@implementation $1

$0

@end
