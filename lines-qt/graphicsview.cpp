#include "graphicsview.h"

GraphicsView::GraphicsView(QGraphicsScene *scene) :
    QGraphicsView(scene)
{
}

void GraphicsView::mouseReleaseEvent(QMouseEvent *event)
{
    emit mouseReleased(event);
}
