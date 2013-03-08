#ifndef GRAPHICSVIEW_H
#define GRAPHICSVIEW_H

#include <QGraphicsView>

class GraphicsView : public QGraphicsView
{
    Q_OBJECT
public:
    explicit GraphicsView(QGraphicsScene *scene);

signals:
    void mouseReleased(QMouseEvent *);

protected:
    void mouseReleaseEvent(QMouseEvent *);
    
public slots:
    
};

#endif // GRAPHICSVIEW_H
